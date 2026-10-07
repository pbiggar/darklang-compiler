/* Convert OCaml's portable signal constants back to host process exit codes. */
#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/signals.h>
CAMLprim value dark_execution_signal_number(value signal) {
  return Val_int(caml_convert_signal_number(Int_val(signal)));
}
