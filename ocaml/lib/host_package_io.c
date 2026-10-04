/* Native SQLite/HTTP boundaries. Compiler algorithms remain in OCaml. */
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <unicode/ucnv.h>
#include <sqlite3.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static void cache_failure(sqlite3 *db, sqlite3_stmt *statement, int code) {
  char message[1024];
  snprintf(message, sizeof(message), "SQLite Error %d: '%s'.", code,
           db ? sqlite3_errmsg(db) : sqlite3_errstr(code));
  if (statement) sqlite3_finalize(statement);
  if (db) sqlite3_close(db);
  caml_failwith(message);
}
static sqlite3 *open_cache(const char *path) {
  sqlite3 *db = NULL;
  int code = sqlite3_open(path, &db);
  if (code != SQLITE_OK) cache_failure(db, NULL, code);
  code = sqlite3_busy_timeout(db, 5000);
  if (code != SQLITE_OK) cache_failure(db, NULL, code);
  code = sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS package_responses "
                        "(cache_key TEXT PRIMARY KEY, status INTEGER NOT NULL, body TEXT NOT NULL)",
                      NULL, NULL, NULL);
  if (code != SQLITE_OK) cache_failure(db, NULL, code);
  return db;
}
static void bind_text(sqlite3 *db, sqlite3_stmt *statement, int index, value text) {
  int code = sqlite3_bind_text64(statement, index, String_val(text),
                               caml_string_length(text), SQLITE_TRANSIENT, SQLITE_UTF8);
  if (code != SQLITE_OK) cache_failure(db, statement, code);
}
CAMLprim value dark_package_cache_read(value path, value key) {
  CAMLparam2(path, key);
  CAMLlocal3(result, pair, body);
  sqlite3 *db = open_cache(String_val(path));
  sqlite3_stmt *statement = NULL;
  int code = sqlite3_prepare_v2(db, "SELECT status, body FROM package_responses WHERE cache_key = $key", -1, &statement, NULL);
  if (code != SQLITE_OK) cache_failure(db, statement, code);
  bind_text(db, statement, 1, key);
  code = sqlite3_step(statement);
  if (code == SQLITE_DONE) result = Val_none;
  else if (code == SQLITE_ROW) {
    int status = sqlite3_column_int(statement, 0);
    const char *text = (const char *)sqlite3_column_text(statement, 1);
    int length = sqlite3_column_bytes(statement, 1);
    body = caml_alloc_initialized_string(length, text ? text : "");
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, Val_int(status));
    Store_field(pair, 1, body);
    result = caml_alloc_some(pair);
  } else cache_failure(db, statement, code);
  sqlite3_finalize(statement);
  sqlite3_close(db);
  CAMLreturn(result);
}
CAMLprim value dark_package_cache_write(value path, value key, value status, value body) {
  CAMLparam4(path, key, status, body);
  sqlite3 *db = open_cache(String_val(path));
  sqlite3_stmt *statement = NULL;
  int code = sqlite3_prepare_v2(db,
    "INSERT INTO package_responses(cache_key, status, body) VALUES ($key, $status, $body) "
    "ON CONFLICT(cache_key) DO UPDATE SET status = excluded.status, body = excluded.body",
    -1, &statement, NULL);
  if (code != SQLITE_OK) cache_failure(db, statement, code);
  bind_text(db, statement, 1, key);
  code = sqlite3_bind_int(statement, 2, Int_val(status));
  if (code != SQLITE_OK) cache_failure(db, statement, code);
  bind_text(db, statement, 3, body);
  code = sqlite3_step(statement);
  if (code != SQLITE_DONE) cache_failure(db, statement, code);
  sqlite3_finalize(statement);
  sqlite3_close(db);
  CAMLreturn(Val_unit);
}
CAMLprim value dark_package_resolve_url(value server, value path) {
  CAMLparam2(server, path);
  CAMLlocal1(result);
  CURLU *url = curl_url();
  if (!url) caml_raise_out_of_memory();
  CURLUcode code = curl_url_set(url, CURLUPART_URL, String_val(server), 0);
  if (code == CURLUE_OK) code = curl_url_set(url, CURLUPART_URL, String_val(path), 0);
  char *text = NULL;
  if (code == CURLUE_OK) code = curl_url_get(url, CURLUPART_URL, &text, 0);
  if (code != CURLUE_OK) { curl_url_cleanup(url); caml_invalid_argument(curl_url_strerror(code)); }
  result = caml_copy_string(text);
  curl_free(text);
  curl_url_cleanup(url);
  CAMLreturn(result);
}
struct response_body { char *data; size_t length; };
static size_t collect_body(char *bytes, size_t size, size_t count, void *context) {
  struct response_body *body = context;
  if (size && count > SIZE_MAX / size) return 0;
  size_t length = size * count;
  if (length > SIZE_MAX - body->length - 1) return 0;
  char *data = realloc(body->data, body->length + length + 1);
  if (!data) return 0;
  body->data = data;
  memcpy(data + body->length, bytes, length);
  body->length += length;
  data[body->length] = 0;
  return length;
}
static void dispose_client(value handle) {
  CURL **client = (CURL **)Data_custom_val(handle);
  if (*client) { curl_easy_cleanup(*client); *client = NULL; }
}
static const struct custom_operations client_operations = {
  .identifier = "dark.package.http-client",
  .finalize = dispose_client,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};
CAMLprim value dark_package_http_create(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(handle);
  static int initialized = 0;
  if (!initialized) { if (curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK) caml_failwith("Unable to initialize HTTP client"); initialized = 1; }
  handle = caml_alloc_custom_mem(&client_operations, sizeof(CURL *), 4096);
  CURL *client = curl_easy_init();
  *(CURL **)Data_custom_val(handle) = client;
  if (!client) caml_raise_out_of_memory();
  curl_easy_setopt(client, CURLOPT_TIMEOUT_MS, 30000L);
  curl_easy_setopt(client, CURLOPT_FOLLOWLOCATION, 1L);
  curl_easy_setopt(client, CURLOPT_MAXREDIRS, 50L);
  curl_easy_setopt(client, CURLOPT_NOSIGNAL, 1L);
  curl_easy_setopt(client, CURLOPT_COOKIEFILE, "");
  CAMLreturn(handle);
}
CAMLprim value dark_package_http_dispose(value handle) {
  CAMLparam1(handle);
  dispose_client(handle);
  CAMLreturn(Val_unit);
}
CAMLprim value dark_package_http_get(value handle, value url) {
  CAMLparam2(handle, url);
  CAMLlocal4(result, body_string, content_type, content_value);
  CURL *client = *(CURL **)Data_custom_val(handle);
  if (!client) caml_invalid_argument("HTTP client has been disposed");
  struct response_body body = { NULL, 0 };
  char error[CURL_ERROR_SIZE] = {0};
  curl_easy_setopt(client, CURLOPT_URL, String_val(url));
  curl_easy_setopt(client, CURLOPT_WRITEFUNCTION, collect_body);
  curl_easy_setopt(client, CURLOPT_WRITEDATA, &body);
  curl_easy_setopt(client, CURLOPT_ERRORBUFFER, error);
  CURLcode code = curl_easy_perform(client);
  curl_easy_setopt(client, CURLOPT_WRITEDATA, NULL);
  curl_easy_setopt(client, CURLOPT_ERRORBUFFER, NULL);
  if (code != CURLE_OK) { free(body.data); caml_failwith(error[0] ? error : curl_easy_strerror(code)); }
  long status = 0;
  curl_easy_getinfo(client, CURLINFO_RESPONSE_CODE, &status);
  char *type = NULL;
  curl_easy_getinfo(client, CURLINFO_CONTENT_TYPE, &type);
  if (type) { content_value = caml_copy_string(type); content_type = caml_alloc_some(content_value); }
  else content_type = Val_none;
  body_string = caml_alloc_initialized_string(body.length, body.data ? body.data : "");
  free(body.data);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_long(status));
  Store_field(result, 1, body_string);
  Store_field(result, 2, content_type);
  CAMLreturn(result);
}
CAMLprim value dark_package_decode(value charset, value bytes) {
  CAMLparam2(charset, bytes);
  CAMLlocal1(result);
  UErrorCode error = U_ZERO_ERROR;
  int32_t required = ucnv_convert("UTF-8", String_val(charset), NULL, 0,
    String_val(bytes), caml_string_length(bytes), &error);
  if (error != U_BUFFER_OVERFLOW_ERROR && U_FAILURE(error)) caml_invalid_argument(u_errorName(error));
  error = U_ZERO_ERROR;
  char *decoded = malloc((size_t)required + 1);
  if (!decoded) caml_raise_out_of_memory();
  int32_t length = ucnv_convert("UTF-8", String_val(charset), decoded, required + 1,
    String_val(bytes), caml_string_length(bytes), &error);
  if (U_FAILURE(error)) { free(decoded); caml_invalid_argument(u_errorName(error)); }
  result = caml_alloc_initialized_string(length, decoded);
  free(decoded);
  CAMLreturn(result);
}
