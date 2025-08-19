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

Yes. It waits for two things to happen:

1. The browser's `document.readyState` becomes `complete`.

1. Two screenshots, taken 100ms apart, are the same.

If a page never meets these conditions, `see` will hang. Long time no see.

> Does this tool return HTML?

No. A human sees rendered text, not a mess of HTML tags. The tool mimics this by returning the `document.title` and `document.body.innerText` as a single block of text. This saves tokens for a large language model.

> Does an anti-bot challenge page cause `see` to fail?

No. A challenge page won't cause `see` to fail. It's your script's job to check the output and decide what to do.

> Does `see` fake mouse movements to avoid bot detection?

No. For low-volume, slow-frequency access, I hope it won't be necessary.

> Does this tool let you select parts of a page with CSS selectors?

No. The tool has one job: return the `document.title` and `document.body.innerText`. The whole point is to get the full context for an LLM.

## CLI

> Does the URL argument need to be quoted?

No. This follows the convention of tools like `curl` and `wget`.
