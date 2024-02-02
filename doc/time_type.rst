=========
Time type
=========

A simple usage is:

  .. code:: ada


     Put_Line (Format_Time ("%Y-%m-%d %H:%M", Clock));

Format string describes date and time output format. The string is a set of standard
character and special tag that are replaced by the corresponding values. A syntax
of a format string is:

  ``%[flag]specifier``

Here are the recognized directives:

  .. table::
     :widths: 20 80

     ===  ========================
     %%   A literal ``%`` character.
     \\n  A newline.
     \\t  A horizontal tab.
     ===  ========================

Flags
-----

The following flag characters are permitted:

``-`` (hyphen)
  Do not pad a numeric result string.

``_`` (underscore)
  Pad a numeric result string with spaces.

``0`` (zero)
  Pad a numeric result string with zeros.

Specifiers
----------

The following format specifiers are available:

``%a``
  The abbreviated name of the day of the week according to the current locale.

``%A``
  The full name of the day of the week according to the current locale.

``%b``
  The abbreviated month name according to the current locale.

``%B``
  The full month name according to the current locale.

``%c``
  The preferred date and time representation for the current locale.

``%C``
  The century number (year/100) as a 2-digit integer.

``%d``
  The day of the month as a decimal number (01 .. 31).

``%D``
  Equivalent to ``%m/%d/%y``.

``%e``
  Like ``%d``, the day of the month as a decimal number, but a leading zero is
  replaced by a space ( 1 .. 31).

``%F``
  Equivalent to ``%Y-%m-%d`` (the ISO 8601 date format).

``%h``
  Equivalent to ``%b``.

``%H``
  The hour as a decimal number using a 24-hour clock (00 .. 23).

``%I``
  The hour as a decimal number using a 12-hour clock (01 .. 12).

``%j``
  The day of the year as a decimal number (001 .. 366).

``%k``
   The hour (24-hour clock) as a decimal number ( 0 .. 23). Single digits are
   preceded by a blank.

``%l``
  The hour (12-hour clock) as a decimal number ( 1 .. 12). Single digits are
  preceded by a blank.

``%m``
  The month as a decimal number (01 .. 12).

``%M``
  The minute as a decimal number (00 .. 59).

``%n``
  A newline character.

``%p``
  Either "AM" or "PM" according to the given time value, or the corresponding
  strings for the current locale. Noon is treated as "PM" and midnight as "AM".

``%P``
  Like ``%p`` but in lowercase: "am" or "pm" or a corresponding string for the
  current locale.

``%r``
  The time in 12-hour notation. (In the POSIX locale this is equivalent to ``%I:%M:%S %p``.)

``%R``
  The time in 24-hour notation (``%H:%M``).

``%s``
  The number of seconds since the Epoch, 1970-01-01 00:00:00 +0000 (UTC).

``%S``
  The second as a decimal number (00 .. 59).

``%t``
  A tab character.

``%T``
  The time in 24-hour notation (``%H:%M:%S``).

``%u``
  The day of the week as a decimal (1 .. 7), Monday being 1.

``%U``
  The week number of the current year as a decimal number (00 .. 53), starting
  with the first Sunday as the first day of week 01.

``%w``
  The day of the week as a decimal (0 .. 6), Sunday being 0.

``%W``
  The week number of the current year as a decimal number (00 .. 53), starting
  with the first Monday as the first day of week 01.

``%x``
  The preferred date representation for the current locale without the time.
  (In the POSIX locale this is equivalent to ``%m/%d/%y``.)

``%X``
  The preferred time representation for the current locale without the date.
  (In the POSIX locale this is equivalent to ``%H:%M:%S``.)

``%y``
  The year as a decimal number without a century (00 .. 99).

``%Y``
  The year as a decimal number including the century.

``%z``
  The +hhmm or -hhmm numeric timezone (that is, the hour and minute offset from UTC).

``%Z``
  The timezone name (e.g., UTC+02:00)

Additional time fields:

``%i``
  The milliseconds as a decimal number (000 .. 999).

``%o``
  The microseconds as a decimal number (000000 .. 999999).

``%N``
  The nanoseconds as a decimal number (000000000 .. 999999999).

Modifiers:

``E``
  Use alternative ("era‐based") format.

``O``
  Use alternative numeric symbols.
