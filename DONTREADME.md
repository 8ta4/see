# see

## Goals

### Be Seen as Human

> Does this tool use Playwright?

No. Controlling a browser from the outside may leave a fingerprint. [`puppeteer-extra`](https://github.com/berstend/puppeteer-extra) hasn't been updated in a while.

> Does this tool use Chrome's remote debugging port?

No. Connecting via the remote debugging port can leave a fingerprint.

Instead, `see` talks to a companion Chrome extension. Since it runs in your day-to-day browser, requests use your fingerprint and look like normal browsing activity.

> Does the extension connect to a local HTTP server to talk to the CLI?

No.

- Opening a network port is a security risk I'd rather not take.

- It's also a polling nightmare. Truly appalling. The extension has to ask, "Is the server even running?" and then it has to keep asking, "Do you have a job for me?"

> Does the extension connect to a local WebSocket server to talk to the CLI?

No.

- You're opening a network port. So that security risk is on the table.

- A WebSocket solves the second polling problem: "Do you have a job for me?" But you're still stuck with the first one: "Is the server even running?"

> Does the CLI connect to a WebSocket on the Native Messaging host?

No. That would mean opening a network port, which is a security risk.

Instead, it just uses a UNIX domain socket.

> Does `see` fake mouse movements?

No.

- A real mouse move is `isTrusted: true`, but a fake one is `false`. A mix of both for the same input type is a weird signal.

- If `see` moves the cursor and then you move your real mouse, an unnatural jump can be detected.

> Does `see` fake scrolls?

No. A Vimium user's scroll is untrusted, but it's explained by a key press with `isTrusted: true` that came first. `see` can't fake that initial trusted event.

> Does `see` use my active tab?

No. Hijacking the tab you're potentially using is disruptive. `see` creates a new tab for each job instead.

> Does `see` close tabs?

Yes, but not right away. Closing a tab the second you're done with it is a classic bot move.

For one, it looks way more human to the site you're visiting. To them, your session isn't over, it's just backgrounded. It looks like you're multitasking.

Plus, there's the chance that two sites you're scraping both use something like Cloudflare. If you reuse the same tab to jump between them, you're leaving a clear signal. A human might jump between sites like that once, sure. But doing it repeatedly, every few seconds starts to look like a pattern.

But leaving tabs open forever gets messy. It's a great way to exhaust your resources and clutter your browser. So `see` closes the tab after a delay. That delay is sampled from a log-normal distribution to mimic human workflow.

> Does `see` add its own delay?

No. That's your script's job, not the tool's. Your script knows if you're hitting a site that needs you to slow down. If I baked in a delay, it would just make `see` feel sluggish for no reason on sites that don't need it.

> Does `see` automate logins?

No. `see` uses your real browser session. You log in first. Then you run the command. `see` will see the page just like you do.

> Does `see` deal with cookie banners?

No. That's your job. You click accept once, and `see` uses your session. It's the same deal as logging in.

### See as Human

> Does this tool wait for the page to load?

Yes. It'll grab the text as soon as one of two things happens:

- Two consecutive screenshots become identical after the browser's `readyState` is `complete`.

- 10 seconds go by since the navigation started.

I put that 10-second limit in because anything more starts to feel broken.

> Won't a never-ending animation cause a timeout?

Yep.

If you're on a page for the first time, that 10-second wait is fine. `see` can't tell the difference between a harmless animation and a loading spinner, so it just waits.

If you're hitting the same page over and over and that delay is bugging you, that's your cue to fix it. Use an extension like uBlock Origin to hide the animation.

> Does `see` check network requests to see if the page is done loading?

No.

A network call could just be some analytics ping that changes nothing on the screen.

A quiet network doesn't mean JavaScript isn't in the middle of some heavy lifting to build the page.

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

> Does `see` exit with an error on a 404?

No. If your browser hits a 404, it shows that error page. `see` just grabs the text from that page. It's on your script to look at the output and decide what to do.

> Does an anti-bot challenge page cause `see` to exit with an error?

No. `see` just sees what's there, and gives you its text. It's your script's job to check that output and decide what to do.

> Does this tool let you select parts of a page with CSS selectors?

No. The tool has one job: return the `document.title` and `document.body.innerText`.

`see` isn't for surgically extracting specific text. The whole point is to give an LLM the big-picture context of a page.

> Does `see` click things to make more text appear?

No. The tool is called `see`, not `do`. It doesn't click anything. Its job is to report what's visible when the page loads. That's it.

> Does `see` scroll down to load more content?

No. `see` doesn't scroll.

## CLI

> Does the URL argument need to be quoted?

No. This follows the convention of tools like `curl` and `wget`.
