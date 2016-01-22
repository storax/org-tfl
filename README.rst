=======
org-tfl
=======

Transport for London meets Emacs Orgmode

This is an experimental project to use the `Transport for London (TfL) API <https://api-portal.tfl.gov.uk/docs>`_ in combination with Emacs orgmode.

At the moment there are 2 main interactive funtions:

* ``org-tfl-jp`` will display the result in a buffer
* ``org-tfl-jp-org`` will create a special heading with a link.
  Everytime you open the link, the result is updated.
  You can use the regular schedule function of org-mode to change the time.

.. CAUTION:: This is a very hacky software, written by a complete lisp newbie.
	     The author is aware that the code is ugly, bad, dangerous
	     and needs a lot of work.
