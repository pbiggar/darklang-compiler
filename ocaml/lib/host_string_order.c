/* host_string_order.c - Allocation-free ordinal UTF-16 comparison of WTF-8 keys. */
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

struct unit_stream {
  const unsigned char *bytes;
  mlsize_t length;
  mlsize_t offset;
  int pending;
};

/* Mirror HostText.utf16Units: surrogate scalars are legal internal WTF-8,
   but overlong encodings, broken continuations and out-of-range values are not. */
static int next_unit(struct unit_stream *stream) {
  if (stream->pending >= 0) {
    int unit = stream->pending;
    stream->pending = -1;
    return unit;
  }
  if (stream->offset == stream->length) return -1;
  unsigned int first = stream->bytes[stream->offset++];
  if (first < 0x80) return (int)first;
  unsigned int count, scalar, minimum;
  if ((first & 0xe0) == 0xc0) { count = 2; scalar = first & 0x1f; minimum = 0x80; }
  else if ((first & 0xf0) == 0xe0) { count = 3; scalar = first & 0x0f; minimum = 0x800; }
  else if ((first & 0xf8) == 0xf0) { count = 4; scalar = first & 0x07; minimum = 0x10000; }
  else { caml_invalid_argument("Malformed UTF-8 host text"); }
  if (count - 1 > stream->length - stream->offset)
    caml_invalid_argument("Malformed UTF-8 host text");
  for (unsigned int index = 1; index < count; ++index) {
    unsigned int byte = stream->bytes[stream->offset++];
    if ((byte & 0xc0) != 0x80) caml_invalid_argument("Malformed UTF-8 host text");
    scalar = (scalar << 6) | (byte & 0x3f);
  }
  if (scalar < minimum || scalar > 0x10ffff)
    caml_invalid_argument("Malformed UTF-8 host text");
  if (scalar < 0x10000) return (int)scalar;
  scalar -= 0x10000;
  stream->pending = (int)(0xdc00 | (scalar & 0x3ff));
  return (int)(0xd800 | (scalar >> 10));
}

CAMLprim value dark_compare_utf16(value left, value right) {
  CAMLparam2(left, right);
  struct unit_stream a = {(const unsigned char *)String_val(left), caml_string_length(left), 0, -1};
  struct unit_stream b = {(const unsigned char *)String_val(right), caml_string_length(right), 0, -1};
  int order = 0, x, y;
  /* Continue after a difference: the old decoder validates entire strings,
     including malformed suffixes and the longer side of prefix comparisons. */
  do {
    x = next_unit(&a);
    y = next_unit(&b);
    if (order == 0 && x != y) order = x < y ? -1 : 1;
  } while (x >= 0 || y >= 0);
  CAMLreturn(Val_int(order));
}
