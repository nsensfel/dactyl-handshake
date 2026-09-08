# Dactyl ManuForm Handshake Keyboard

This is a fork of the [Dactyl-ManuForm](https://github.com/tshort/dactyl-keyboard) (itself a fork of
 [Dactyl](https://github.com/adereth/dactyl-keyboard) adding the thumb cluster from [ManuForm](https://github.com/jeffgran/ManuForm))

The script has been greatly altered for readability and ease of customization,
but no efforts have been made to keep features from parent codebases. Despite
this, no authors were removed, regardless of whether their contributions remain,
as this is still based on their work.

## Features
This fork explores the possibility of a thumb cluster meant to be used the same
way as those on computer mice. That is to say, if the other fingers are pushing
buttons vertically, the thumb is pushing horizontally.

The current strategy is to have the thumb cluster fairly high, creating a wall
between keyboard finds itself separating the thumb from the index, much like
during a handshake.

A somewhat different approach for palm rests is also being tested, which could
be described as closer to a bike handle (or flight throttle rudder) than usual:
instead of a flat surface on which the bottom of the palm rests, the cusp of the
key well provides a surface for the middle and upper part of the palm.

The project is in its **early design phase**, definitely not ready for use.

## Generate OpenSCAD and STL models

* Run `lein generate` or `lein auto generate`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files


## License

Copyright © 2015-2026 Nathanaël Sensfelder, Matthew Adereth, Tom Short, and Leo Lou

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
