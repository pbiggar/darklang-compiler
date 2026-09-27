# Sample programs

## HTTP HTML fetcher

Build and run the standalone Dark program:

```sh
./build --ai
./dark samples/http-fetch.dark -o /tmp/http-fetch
/tmp/http-fetch http://example.com/
```

Pass any plain `http://` URL, including a local server URL. The program prints
the response status and indents the HTML for inspection. It uses the explicit
trusted client API because the URL comes from the command line; HTTPS is not yet
supported by the Dark HTTP client.
