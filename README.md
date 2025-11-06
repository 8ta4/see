# see

## See and Be Seen as Human

> What is this tool about?

`see` is a CLI tool that gets text from a web page via its extension on your browser.

The goal is to see and be seen as human. There are no ifs, ands, or bots.

## Setup

> How do I set up `see`?

1. Make sure you're using a Mac.

1. Install [devenv](https://github.com/cachix/devenv/blob/0a1a32c2b68be15676304a489e86a2445815d93e/docs/getting-started.md#installation).

1. Install [direnv](https://github.com/cachix/devenv/blob/0a1a32c2b68be15676304a489e86a2445815d93e/docs/automatic-shell-activation.md#installing-direnv).

1. Install [Homebrew](https://brew.sh/#install).

1. Open a terminal window.

1. Run the following commands:

   ```sh
   brew install 8ta4/see/see
   see -h
   git clone git@github.com:8ta4/see.git
   direnv allow see
   cd see
   install
   release
   ```

1. [Load the unpacked extension](https://developer.chrome.com/docs/extensions/get-started/tutorial/hello-world#load-unpacked) from the `cljs/release` directory.

## Usage

> How do I use this tool?

You give it a URL, and it prints the text.

```bash
see https://example.com
```
