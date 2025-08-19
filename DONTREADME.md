# see

## Goals

### Be Seen as Human

> Does this tool use Playwright?

Controlling a browser from the outside may leave a fingerprint.

> Does this tool use Chrome's remote debugging port?

Connecting via the remote debugging port can leave a fingerprint.

Instead, `see` talks to a companion Chrome extension. Since it runs in your day-to-day browser, requests use your fingerprint and look like normal browsing activity.

> Does the CLI use a local HTTP server to talk to the extension?

No, that's a polling nightmare.

First, the extension has to constantly ask, "Is the server even running yet?" Then, once it connects, it has to keep asking, "Do you have a job for me yet?"

> Does the CLI use a local WebSocket server to talk to the extension?

A WebSocket can push jobs without the second layer of polling. But it still has the same "are you there?" problem.

`see` uses Chrome's native messaging.

### See as Human

> Does this tool wait for the page to load?

Yes. It waits for two things to happen:

1. The browser's `document.readyState` becomes `complete`.

1. Two screenshots, taken 100ms apart, are the same.

If a page never meets these conditions, `see` will hang. Long time no see.

> Does `see` use Readability.js to find the main content?

No. Readability.js is too aggressive. It throws away things you might need.

> Does `see` use Turndown to convert HTML to Markdown?

No. Turndown can leave in junk.

> Does this tool return HTML?

No. A human sees rendered text, not a mess of HTML tags.

> Does this tool return `textContent`?

No. `textContent` It has two problems:

- ["`textContent` gets the content of all elements, including `<script>` and `<style>` elements."](https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent#:~:text=textContent%20gets%20the%20content%20of%20all%20elements%2C%20including%20%3Cscript%3E%20and%20%3Cstyle%3E%20elements.)

- ["`textContent` returns every element in the node."](https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent#:~:text=textContent%20returns%20every%20element%20in%20the%20node.) So it'll happily give you text from elements that are hidden.

That's why the tool returns the `document.title` and `document.body.innerText` as a single block of text. This saves tokens for a large language model.

> Does an anti-bot challenge page cause `see` to fail?

No. A challenge page won't cause `see` to fail. It's your script's job to check the output and decide what to do.

> Does `see` fake mouse movements to avoid bot detection?

No. For low-volume, low-frequency access, I hope it won't be necessary.

> Does this tool let you select parts of a page with CSS selectors?

No. The tool has one job: return the `document.title` and `document.body.innerText`. The whole point is to get the full context for an LLM.

## CLI

> Does the URL argument need to be quoted?

No. This follows the convention of tools like `curl` and `wget`.
