#include <stdint.h>

#define IMPORT(a, b) __attribute__((import_module(a), import_name(b)))

typedef uint64_t ExtismPointer;

#define DEFINE(name, t, ...)                                                   \
  IMPORT("extism:host/env", #name) extern t _##name(__VA_ARGS__);

DEFINE(input_length, uint64_t)
uint64_t extism_input_length() { return _input_length(); }

DEFINE(length, uint64_t, ExtismPointer)
uint64_t extism_length(ExtismPointer p) { return _length(p); }

DEFINE(alloc, ExtismPointer, uint64_t)
uint64_t extism_alloc(uint64_t n) { return _alloc(n); }

DEFINE(free, void, ExtismPointer)
void extism_free(uint64_t n) { return _free(n); }

DEFINE(input_load_u8, uint8_t, ExtismPointer)
uint8_t extism_input_load_u8(ExtismPointer p) { return _input_load_u8(p); }

DEFINE(input_load_u64, uint64_t, ExtismPointer)
uint64_t extism_input_load_u64(ExtismPointer p) { return _input_load_u64(p); }

DEFINE(output_set, void, ExtismPointer, uint64_t)
void extism_output_set(ExtismPointer p, uint64_t n) {
  return _output_set(p, n);
}

DEFINE(error_set, void, ExtismPointer)
void extism_error_set(ExtismPointer p) { _error_set(p); }

DEFINE(config_get, ExtismPointer, ExtismPointer)
ExtismPointer extism_config_get(ExtismPointer p) { return _config_get(p); }

DEFINE(var_get, ExtismPointer, ExtismPointer)
ExtismPointer extism_var_get(ExtismPointer p) { return _var_get(p); }

DEFINE(var_set, void, ExtismPointer, ExtismPointer)
void extism_var_set(ExtismPointer k, ExtismPointer v) { return _var_set(k, v); }

DEFINE(store_u8, void, ExtismPointer, uint8_t)
void extism_store_u8(ExtismPointer p, uint8_t x) { return _store_u8(p, x); }

DEFINE(load_u8, uint8_t, ExtismPointer)
uint8_t extism_load_u8(ExtismPointer p) { return _load_u8(p); }

DEFINE(store_u64, void, ExtismPointer, uint64_t)
void extism_store_u64(ExtismPointer p, uint64_t x) { return _store_u64(p, x); }

DEFINE(load_u64, uint64_t, ExtismPointer)
uint64_t extism_load_u64(ExtismPointer p) { return _load_u64(p); }

DEFINE(http_request, ExtismPointer, ExtismPointer, ExtismPointer)
ExtismPointer extism_http_request(ExtismPointer req, ExtismPointer body) {
  return _http_request(req, body);
}

DEFINE(http_status_code, int32_t)
int32_t extism_http_status_code() { return _http_status_code(); }

DEFINE(log_info, void, ExtismPointer)
void extism_log_info(ExtismPointer p) { return _log_info(p); }
DEFINE(log_debug, void, ExtismPointer)
void extism_log_debug(ExtismPointer p) { return _log_debug(p); }
DEFINE(log_warn, void, ExtismPointer)
void extism_warn_info(ExtismPointer p) { return _log_warn(p); }
DEFINE(log_error, void, ExtismPointer)
void extism_log_error(ExtismPointer p) { return _log_error(p); }
