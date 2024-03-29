# dunai-gui-virtual
A GUI build using Dunai and dear-imgui. Working Title.

# AFRP & GUI
AFRP seems to get the short end of the stick when it comes to GUI libraries.
Realistically, there is not much in the way of designing a quality GUI library
for a popular AFRP library. Perhaps the biggest issue is the need for either
monads or comonads, both of which Yampa lack. Dunai solves the issue by
introducing Monadic Signal Functions (MSFs).

A similar problem is retained state. Libraries like GTK and Qt are "retained"
mode graphics libraries. Usually, these libraries demand control of the main
loop. This presents a problem when using functions like `reactimate`, which
*also* want control over the main loop. Dunai does provide an interface to solve
this, but the trouble is then dealing with signals. Instead, immediate mode GUIs
provide a much nicer experience with libraries that want control over the main
loop.

# Building
## Nix
Use `nix develop` to build the flake.
