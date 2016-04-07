=======
org-tfl
=======

Use the Transport For London API in Emacs, powered org-mode.

This is an experimental project to use the `Transport for London (TfL) API <https://api-portal.tfl.gov.uk/docs>`_ in combination with Emacs orgmode.

Installation
------------

org-tfl is available on `MELPA <https://melpa.org>`_.
At the moment svg support is required!

Add the following to your emacs init file::

  (require 'org-tfl)

Screencast
----------

.. figure:: https://raw.github.com/storax/org-tfl/master/screencast1.gif

   Plan a journey and inspect the result in a buffer.
   View maps via google static maps.

.. figure:: https://raw.github.com/storax/org-tfl/master/screencast2.gif

   Plan a journey and create a special link in org-mode.
   Loading the link will refresh the result.

Commands
---------

Below are complete command list:

* ``org-tfl-jp``
   Plan a journey and view the result in a buffer.
* ``org-tfl-jp-org``
   Plan a journey and insert a subheading with a special link.
   The content is the journey result. Open the link to update it.
   Use the scheduling function of org mode to change the date.
   All other options are set via properties.

Customizable Options
---------------------

Below are customizable option list:

* ``org-tfl-api-id``
  Your Application ID for the TfL API. You don't need one
  for personal use. It's IP locked anyway.
* ``org-tfl-api-key``
  Your Application KEY for the TfL API. You don't need one
  for personal use. It's IP locked anyway.
* ``org-tfl-map-width``
  The width in pixels of static maps.
* ``org-tfl-map-height``
  The height in pixels of static maps.
* ``org-tfl-map-type``
  The map type. E.g. "roadmap", "terrain", "satellite", "hybrid".
* ``org-tfl-map-path-color``
  The color of the path of static maps.
* ``org-tfl-map-path-weight``
  The storke weight of paths of static maps.
* ``org-tfl-map-start-marker-color``
  The path color of static maps.
* ``org-tfl-map-start-marker-color``
  The start marker color of static maps.
* ``org-tfl-map-end-marker-color``
  The end marker color of static maps.
* ``org-tfl-time-format-string``
  The format string to display time.
* ``org-tfl-date-format-string``
  The format string to display dates.

History
-------

0.1.0 (2016-01-22)
+++++++++++++++++++++++++++++++++++++++

* Initial release

0.2.0 (2016-01-22)
+++++++++++++++++++++++++++++++++++++++

* Add static google maps with customization
* Fix formatting

0.2.1 (2016-01-22)
+++++++++++++++++++++++++++++++++++++++

* Fix Package Header

0.2.2 (2016-01-22)
+++++++++++++++++++++++++++++++++++++++

* Remove url-http-parse-headers override

0.2.3 (2016-01-25)
+++++++++++++++++++++++++++++++++++++++

* Fix cl-lib usage (thanks Soyhei YOSHIDA)
* Revert remobal of url-http-parse-headers override

0.3.0 (2016-01-29)
+++++++++++++++++++++++++++++++++++++++

* Remove helm dependency and use ``completing-read``.

0.3.1 (2016-01-31)
+++++++++++++++++++++++++++++++++++++++

* Fix disambiguation check with VIA parameter.

0.3.2 (2016-02-24)
+++++++++++++++++++++++++++++++++++++++

* Use ampersand for Hammersmith & City line.

0.3.3 (2016-04-07)
+++++++++++++++++++++++++++++++++++++++

* Fix dlr icon size
