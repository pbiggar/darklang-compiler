# Sample programs

## HTTP HTML fetcher

Build and run the standalone Dark program:

```sh
./build --ai
./dark samples/http-fetch.dark -o /tmp/http-fetch
/tmp/http-fetch http://example.com/
```

Pass any plain `http://` URL, including a local server URL. This is a diagnostic
tool: it prints URL parsing, DNS and connection attempts, request and response
details, then indents the HTML for inspection. It uses `HTTP_PROXY` (or
`http_proxy`) when set, except for hosts matched by `NO_PROXY` (or `no_proxy`).
Both paths use Dark DNS, TCP, and HTTP framing directly to show connection and
response errors. HTTPS is not yet supported by the Dark HTTP stack.
