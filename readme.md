# mullvad

## Introduction

This simple Emacs package collects a few functions for interfacing with [Mullvad](https://mullvad.net/), a VPN service.

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/internet-archive")
```

where `"path/to/internet-archive"` is the path to the local repository you just cloned.

### Elpaca/Straight

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, you just need to add this your `init.el` file:

``` emacs-lisp
(use-package mullvad
  :elpaca (mullvad
           :host github
	   :repo "benthamite/mullvad")
  :demand t)
```

If you use [straight](https://github.com/radian-software/straight.el), just replace `:elpaca` with `:straight` in the formula above.

## Configuration

To use this package, you must have `mullvad` installed and have run `mullvad account login [your account number]` in the terminal. To find your account number, go to "settings" > "account" in the graphical interface.

To decide which server it should connect to, the package will prompt the user for a city or a website, and select the corresponding server as specified by the association lists `mullvad-cities-and-servers` and `mullvad-websites-and-cities`. You should set these user options accordingly. Here is an example, taken from my [configuration](https://github.com/benthamite/dotfiles/blob/master/emacs/config.org#mullvad):

``` emacs-lisp
(setq mullvad-cities-and-servers
	'(("London" . "gb-lon-wg-001")
	  ("Madrid" . "es-mad-wg-101")
	  ("Malmö" . "se-sto-wg-001")
	  ("Frankfurt" . "de-fra-wg-001")
	  ("New York" . "us-nyc-wg-601")
	  ("São Paulo" . "br-sao-wg-001")))

(setq mullvad-websites-and-cities
	'(("Criterion Channel" . "New York")
	  ("HathiTrust" . "New York")
	  ("Library Genesis" . "Malmö")
	  ("Pirate Bay" . "Malmö")
	  ("Wise" . "Madrid")))
```

You can get a list of servers by running

``` shell
mullvad relay list
```

If you would like to be presented with a list of predefined choices when prompted to select a connection duration, you can customize the user option `mullvad-durations`. For example:

```emacs-lisp
(setq mullvad-durations '(1 5 10 30 60 120))
```

Note that you can still enter a duration not in the list.

To connect without a time limit, just press `RET` without entering or selecting a value.

By default, `mullvad` emits a message every time it connects to a server or disconnects from one. If you would like to inhibit these messages, you san set `mullvad-silent` to `t`.

## Usage

`M-x mullvad` provides a point of entry to the relevant commands:

![The `mullvad` menu](./mullvad-menu.png)

`mullvad-dwim` will prompt the user to connect to a city or a website, if not already connected, and will disconnect otherwise. The remaining commands are self-explanatory.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
