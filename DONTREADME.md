# see

## Goals

### Be Seen as Human

> Does this tool use Playwright?

No. Controlling a browser from the outside may leave a fingerprint. [`puppeteer-extra`](https://github.com/berstend/puppeteer-extra) hasn't been updated in a while.

> Does this tool use Chrome's remote debugging port?

No. Connecting via the remote debugging port can leave a fingerprint.

Instead, `see` talks to a companion Chrome extension. Since it runs in your day-to-day browser, requests use your fingerprint and look like normal browsing activity.

> Does the CLI use a local HTTP server to talk to the extension?

No. that's a polling nightmare. Truly appalling.

First, the extension has to constantly ask, "Is the server even running yet?" Then, once it connects, it has to keep asking, "Do you have a job for me yet?"

> Does the CLI use a local WebSocket server to talk to the extension?

A WebSocket can push jobs without the second layer of polling. But it still has the same "are you there?" problem.

`see` uses Chrome's native messaging.

> Does `see` fake mouse movements?

Yes. `see` uses `ghost-cursor` to move it along some human-like paths. It's cheap insurance.

> Does `see` add its own delay?

No. That's your script's job, not the tool's. Your script knows if you're hitting a site that needs you to slow down. If I baked in a delay, it would just make `see` feel sluggish for no reason on sites that don't need it.

### See as Human

> Does this tool wait for the page to load?

Yes. It'll grab the text as soon as one of two things happens:

- Two consecutive screenshots become identical after the browser's `readyState` is `complete`.

- 10 seconds go by since the navigation started.

I put that 10-second limit in because anything more starts to feel broken.

> How long does `see` wait between screenshots?

It waits 100 milliseconds.

This interval is chosen to make the tool itself feel fast. After a page is visibly done, the last thing you want is for the tool to add its own noticeable delay.

"[0.1 second is about the limit for having the user feel that the system is reacting instantaneously](https://www.nngroup.com/articles/response-times-3-important-limits/#:~:text=0.1%20second%20is%20about%20the%20limit%20for%20having%20the%20user%20feel%20that%20the%20system%20is%20reacting%20instantaneously)".

> Does `see` use Readability.js to find the main content?

No. Readability.js is too aggressive. It throws away things you might need.

> Does `see` use Turndown to convert HTML to Markdown?

No. Turndown can leave in junk.

> Does this tool return HTML?

No. A human sees rendered text, not a mess of HTML tags.

> Does this tool return `textContent`?

No. `textContent` It has two problems:

- "[`textContent` gets the content of all elements, including `<script>` and `<style>` elements.](https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent#:~:text=textContent%20gets%20the%20content%20of%20all%20elements%2C%20including%20%3Cscript%3E%20and%20%3Cstyle%3E%20elements.)"

- "[`textContent` returns every element in the node.](https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent#:~:text=textContent%20returns%20every%20element%20in%20the%20node.)" So it'll happily give you text from elements that are hidden.

That's why the tool returns the `document.title` and `document.body.innerText` as a single block of text. This saves tokens for a large language model.

> Does an anti-bot challenge page cause `see` to exit with an error?

No. `see` just sees what's there, and gives you its text. It's your script's job to check that output and decide what to do.

> Does this tool let you select parts of a page with CSS selectors?

No. The tool has one job: return the `document.title` and `document.body.innerText`. The whole point is to get the full context for an LLM.

## CLI

> Does the URL argument need to be quoted?

No. This follows the convention of tools like `curl` and `wget`.
