=======================
String and scalar types
=======================

A simple usage is:

  .. code:: ada

     Put_Line (-(+"%s" & "A string."));

Function ``"&"`` loads the data from the given value and converts them to character
string equivalents.

A syntax of a format string is:

  ``%[flags][width][.precision]specifier``


Escape sequences in format strings:

  .. table::
     :widths: 10 90

     ===  ===
     \\n  line feed
     \\r  carriage return
     \\b  backspace
     \\t  horizontal tab
     \\f  form feed
     ===  ===

Flags
-----

``<``, ``-``
  The converted value is to be left adjusted on the field boundary.

``>``
  The converted value is to be right adjusted on the field boundary.

``^``
  The converted value is to be center adjusted on the field boundary.

``+``
  A sign (+ or -) should always be placed before a number produced by a signed
  conversion. By default, a sign is used only for negative numbers.

``' '`` (space)
  A blank should be left before a positive number produced by a signed conversion.
  It is ignored if ``+`` flag is present.

``#``
  Used with ``o``, ``x`` or ``X`` specifiers the value is use with Base in C style.
  For ``o`` specifier, the first character of the output string is made zero (by
  prefixing a :code:`0` if it was not zero already). For ``x`` and ``X`` specifiers, a nonzero result
  has the string :code:`"0x"` (or :code:`"0X"` for ``X`` specifier) prepended to it.

``~``
  Used with ``o``, ``x`` or ``X`` specifiers the value is use with Base in Ada
  style *base#number#*.

``0`` (zero)
  The value should be zero padded. It is ignored if ``<`` or ``-`` flag is present.

``_`` (underscore)
  For number conversion (``d``, ``i``, ``u``, ``o``, ``x``, ``X``, ``f``, ``F``, ``g``, ``G``, ``a``, ``A``, ``p``, ``P``)
  the output is to be grouped with grouping character \'_\'. Group size is 3 for Base equal 10, otherwise 4.

``'``
  For decimal conversion (``d``, ``i``, ``u``, ``f``, ``F``, ``g``, ``G``) the output is to be
  grouped with thousands grouping characters if the locale information indicates any.

Specifiers
----------

``d``, ``i``
  Converts a **signed integer** into decimal representation :code:`[-]dddd`.
  Argument is signed integer.

``o``
  Converts an **integer** into octal representation *oooo*.
  Argument is signed or unsigned integer.

``x``, ``X``
  Converts an **integer** into hexadecimal representation :code:`hhhh`. For the ``x`` specifier
  lower case letters is used, and upper case letters for the ``X`` specifier.
  Argument is signed or unsigned integer.

``u``
  Converts a **unsigned integer** into decimal representation :code:`dddd`.
  Argument type is unsigned signed integer.

``f``, ``F``
  Converts **real** number to the decimal notation in the style :code:`[-]ddd.ddd`.
  Argument is floating point or fixed point number.

``g``, ``G``
  Converts **real** number to the decimal notation in the style ``f`` or ``e``
  (``F`` or ``E`` for ``G`` specifier). Trailing zeros are removed from the fractional part
  of the result. If the precision is zero, it is treated as 1.
  Style ``e`` is used if the exponent from its conversion is less than -4 or
  greater than or equal to the precision.
  Argument is floating point or fixed point number.

``e``, ``E``

  Converts **real** number to the decimal exponent notation.
  For the ``e`` specifier style :code:`[-]d.ddde±dd` is used.
  For the ``E`` specifier style :code:`[-]d.dddE±dd` is used.
  Argument is floating point or fixed point number.

``a``, ``A``
  Converts **real** number to the hexadecimal notation.
  For the ``~`` flag style :code:`[-]16#h.hhhh#e±dd` is used.
  For the ``#`` flag style :code:`[-]0xh.hhhhp±d` is used.
  Otherwise the style :code:`[-]16#hhh.hhhh#` is used.
  For the ``a`` specifier lower case letters is used, and upper case letters for
  the ``A`` specifier.
  Argument is floating point or fixed point number.

``s``
  Writes string.

``c``
  Writes single character.

``p``, ``P``
  Pointer address argument is printed in hexadecimal. For the ``p`` specifier
  lower case letters is used, and upper case letters for the ``P`` specifier.

``t``
  Converts **enumeration** value to a string in capitalized style (first
  character of a string in upper case).

``l``
  Converts **enumeration** value to a string in lower case.

``U``
  Converts **enumeration** value to a string in upper case.

``m``
  Converts **enumeration** value to a string in mixed case (all words are capitalized).

Width
-----

``number``
  An optional decimal digit string (with nonzero first digit) specifying a minimum
  field width. If the value to be printed is shorter than this number, the result
  is padded with blank spaces. The value is not truncated even if the result is larger.

``*``
  The width is not specified in the format string, but as an additional integer
  value argument preceding the argument that has to be formatted.

Precision
---------

``.number``
  For integer specifiers ``d``, ``i``, ``o``, ``u``, ``x``, ``X`` precision specifies
  the minimum number of digits to be written. If the value to be written is shorter
  than this number, the result is padded with leading zeros. The value is not
  truncated even if the result is longer.
  If both the converted value and the precision are 0 the output is empty.
  
  For real number specifiers ``f``, ``F``, ``g``, ``G``, ``e``, ``E``, ``a``, ``A`` this is the
  number of digits to be printed after the decimal point. If the precision is missing,
  it is taken as 6 for ``f``, ``F``, ``g``, ``G``, ``e``, ``E``.
  For ``a``, ``A`` specifiers the default precision is sufficient for exact
  representation of the value.
  If the precision is 0, no decimal‐point character appears.
  
  For string specifier ``s`` this is the maximum number of characters to be printed.
  By default all characters are printed.

``.*``
  The precision is not specified in the format string, but as an additional
  integer value argument preceding the argument that has to be formatted.

Note
----
For the number specifiers (``d``, ``i``, ``o``, ``u``, ``x``, ``X``, ``f``, ``g``, ``e``, ``E``, ``a``, ``A``, ``p``, ``P``) default
alignment is right, otherwise is left.
