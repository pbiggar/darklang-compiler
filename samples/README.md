# Sample programs

## HTTP HTML fetcher

Build and run the standalone Dark program:

```sh
./build --ai
./dark samples/http-fetch.dark -o /tmp/http-fetch
/tmp/http-fetch https://example.com/
```

Pass an `http://` or `https://` URL, including a local server URL. This is a diagnostic
tool: it prints URL parsing, request and response details, then indents the
HTML for inspection. Plain HTTP also prints DNS and connection attempts and
uses `HTTP_PROXY` (or `http_proxy`) when set, except for hosts matched by
`NO_PROXY` (or `no_proxy`). HTTPS uses Dark DNS, TCP, and TLS 1.3, and validates
an RSA server certificate
against the system CA bundle. Servers selecting an unsupported key or TLS mode
fail with a network error.
