# see

## Goals

### Be Seen as Human

> Does this tool use Playwright?

Controlling a browser from the outside may leave a fingerprint.

> Does this tool use Chrome's remote debugging port?

Connecting to the browser via the remote debugging port may leave a fingerprint.

`see` uses Chrome's Native Messaging API.

The requests are virtually indistinguishable from a user clicking a link.

### See as Human

> Does this tool wait for the page to load?

Yes. It waits for `document.readyState` to become `complete`.

If a page never reaches this state, long time no see.

> Does this tool return HTML?

No. A human sees rendered text, not a mess of HTML tags. The tool mimics this by returning only the `document.body.innerText`. This saves tokens for a large language model.

> Does this tool let you select parts of a page with CSS selectors?

No. The tool has one job: return the `document.body.innerText`. The whole point is to get the full context for an LLM.

## CLI

> Does the URL argument need to be quoted?

No. This follows the convention of tools like `curl` and `wget`.
