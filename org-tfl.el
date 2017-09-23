;;; org-tfl.el --- Transport for London meets Orgmode

;; Copyright (C) 2015 2016 by David Zuber

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version: 0.4.0
;; Author: storax (David Zuber), <zuber [dot] david [at] gmx [dot] de>
;; URL: https://github.com/storax/org-tfl
;; Package-Requires: ((org "0.16.2") (cl-lib "0.5") (emacs "24.1"))
;; Keywords: org, tfl

;;; Commentary:

;; Use the Transport For London API in Emacs, powered by org-mode.
;; For ambiguous results, `completing-read' is now used instead of helm.

;; Commands:
;;
;; Below are complete command list:
;;
;;  `org-tfl-jp'
;;    Plan a journey and view the result in a buffer.
;;  `org-tfl-jp-org'
;;    Plan a journey and insert a subheading with a special link.
;;    The content is the journey result.  Open the link to update it.
;;    Use the scheduling function of org mode to change the date.
;;    All other options are set via properties.
;;
;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `org-tfl-api-id'
;;    Your Application ID for the TfL API.  You don't need one
;;    for personal use.  It's IP locked anyway.
;;  `org-tfl-api-key'
;;    Your Application KEY for the TfL API.  You don't need one
;;    for personal use.  It's IP locked anyway.
;;  `org-tfl-map-width'
;;    The width in pixels of static maps.
;;  `org-tfl-map-height'
;;    The height in pixels of static maps.
;;  `org-tfl-map-type'
;;    The map type.  E.g. "roadmap", "terrain", "satellite", "hybrid".
;;  `org-tfl-map-path-color'
;;    The color of the path of static maps.
;;  `org-tfl-map-path-weight'
;;    The storke weight of paths of static maps.
;;  `org-tfl-map-start-marker-color'
;;    The path color of static maps.
;;  `org-tfl-map-start-marker-color'
;;    The start marker color of static maps.
;;  `org-tfl-map-end-marker-color'
;;    The end marker color of static maps.
;;  `org-tfl-time-format-string'
;;    The format string to display time.
;;  `org-tfl-date-format-string'
;;    The format string to display dates.

;; Installation:

;; Add the following to your Emacs init file:
;;
;; (require 'org-tfl)

;;; Code:
(require 'url)
(require 'url-http)
(require 'json)
(require 'cl-lib)
(require 'org)
(require 'org-element)

(defgroup org-tfl nil
  "Org mode Transport for London."
  :group 'org)

(defcustom org-tfl-api-id nil
  "The application id for the Transport for London API."
  :type 'string
  :group 'org-tfl)

(defcustom org-tfl-api-key nil
  "The application key for the Transport for London API."
  :type 'string
  :group 'org-tfl)

(defcustom org-tfl-map-width 800
  "The width of static maps."
  :type 'integer
  :group 'org-tfl)

(defcustom org-tfl-map-height 800
  "The height of static maps."
  :type 'integer
  :group 'org-tfl)

(defcustom org-tfl-map-type "roadmap"
  "The type of static maps."
  :options '("roadmap" "terrain" "satellite" "hybrid")
  :group 'org-tfl)

(defcustom org-tfl-map-path-color "0xff0000ff"
  "The path color of static maps."
  :type 'string
  :group 'org-tfl)

(defcustom org-tfl-map-path-weight 5
  "The path weight of static maps."
  :type 'integer
  :group 'org-tfl)

(defcustom org-tfl-map-start-marker-color "blue"
  "The start marker color of static maps."
  :type 'string
  :group 'org-tfl)

(defcustom org-tfl-map-end-marker-color "red"
  "The end marker color of static maps."
  :type 'string
  :group 'org-tfl)

(defvar url-http-end-of-headers nil
  "The location in a buffer of a http response that's at the end of headers.")
(defvar org-tfl-api-base-url "https://api.tfl.gov.uk/"
  "The base url to the TFL API.")
(defvar org-tfl-api-jp "Journey/JourneyResults/%s/to/%s"
  "API endpoint for the journey planner.")

;; Journey Planner context
(defvar org-tfl-jp-arg-from nil)
(defvar org-tfl-jp-arg-to nil)
(defvar org-tfl-jp-arg-via nil)
(defvar org-tfl-jp-arg-nationalSearch nil)
(defvar org-tfl-jp-arg-date nil)
(defvar org-tfl-jp-arg-time nil)
(defvar org-tfl-jp-arg-timeIs "Departing")
(defvar org-tfl-jp-arg-journeyPreference "leasttime")
(defvar org-tfl-jp-arg-mode nil)
(defvar org-tfl-jp-arg-accessibilityPreference nil)
(defvar org-tfl-jp-arg-fromName nil)
(defvar org-tfl-jp-arg-toName nil)
(defvar org-tfl-jp-arg-viaName nil)
(defvar org-tfl-jp-arg-maxTransferMinutes nil)
(defvar org-tfl-jp-arg-maxWalkingMinutes nil)
(defvar org-tfl-jp-arg-walkingSpeed "average")
(defvar org-tfl-jp-arg-cyclePreference nil)
(defvar org-tfl-jp-arg-adjustment nil)
(defvar org-tfl-jp-arg-bikeProficiency nil)
(defvar org-tfl-jp-arg-alternativeCycle nil)
(defvar org-tfl-jp-arg-alternativeWalking t)
(defvar org-tfl-jp-arg-applyHtmlMarkup nil)
(defvar org-tfl-jp-arg-useMultiModalCall nil)

;; Disambiguations
(defvar org-tfl-jp-fromdis nil)
(defvar org-tfl-jp-todis nil)
(defvar org-tfl-jp-viadis nil)

(defvar org-tfl-org-buffer nil)
(defvar org-tfl-org-buffer-point nil)

(defvar org-tlf-from-history nil)
(defvar org-tlf-to-history nil)
(defvar org-tlf-via-history nil)


(cl-defun org-tfl-create-icon (path &optional (asc 80) (text "  "))
  "Return string with icon at PATH displayed with ascent ASC and TEXT."
  (propertize text 'display
              (create-image
               (with-temp-buffer
                 (insert-file-contents path) (buffer-string))
               nil t :ascent asc :mask 'heuristic)))

;; Icons
(defconst org-tfl-icon-cam
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "cam.svg")))
(defconst org-tfl-icon-location
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "location.svg")))
(defconst org-tfl-icon-tube
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "tube.svg")))
(defconst org-tfl-icon-overground
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "overground.svg")))
(defconst org-tfl-icon-bus
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "bus.svg")))
(defconst org-tfl-icon-train
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "train.svg")))
(defconst org-tfl-icon-walking
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "walking.svg")))
(defconst org-tfl-icon-dlr
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "dlr.svg")))
(defconst org-tfl-icon-coach
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "coach.svg")))
(defconst org-tfl-icon-river-bus
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "river-bus.svg")))
(defconst org-tfl-icon-replacement-bus
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "replacement-bus.svg")))
(defconst org-tfl-icon-disruption
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "disruption.svg")))
(defconst org-tfl-icon-information
  (org-tfl-create-icon (concat (file-name-directory load-file-name) "information.svg")))

(defvar org-tfl-mode-icons
  (list
   (cons "coach" org-tfl-icon-coach)
   (cons "overground" org-tfl-icon-overground)
   (cons "river-bus" org-tfl-icon-river-bus)
   (cons "dlr" org-tfl-icon-dlr)
   (cons "bus" org-tfl-icon-bus)
   (cons "replacement-bus" org-tfl-icon-replacement-bus)
   (cons "tube" org-tfl-icon-tube)
   (cons "walking" org-tfl-icon-walking)
   (cons "national-rail" org-tfl-icon-train)
   (cons "tflrail" org-tfl-icon-train)
   (cons "international-rail" org-tfl-icon-train))
  "Mapping of modes to icons.")

(defface org-tfl-bakerloo-face
  '((t (:foreground "white" :background "#996633")))
  "Bakerloo Line Face"
  :group 'org-tfl)

(defface org-tfl-central-face
  '((t (:foreground "white" :background "#CC3333")))
  "Central Line Face"
  :group 'org-tfl)

(defface org-tfl-circle-face
  '((t (:foreground "white" :background "#FFCC00")))
  "Circle Line Face"
  :group 'org-tfl)

(defface org-tfl-district-face
  '((t (:foreground "white" :background "#006633")))
  "District Line Face"
  :group 'org-tfl)

(defface org-tfl-hammersmith-face
  '((t (:foreground "white" :background "#CC9999")))
  "Hammersmith and City Line Face"
  :group 'org-tfl)

(defface org-tfl-jubliee-face
  '((t (:foreground "white" :background "#868F98")))
  "Jubliee Line Face"
  :group 'org-tfl)

(defface org-tfl-metropolitan-face
  '((t (:foreground "white" :background "#660066")))
  "Metropolitan Line Face"
  :group 'org-tfl)

(defface org-tfl-northern-face
  '((t (:foreground "white" :background "#000000")))
  "Northern Line Face"
  :group 'org-tfl)

(defface org-tfl-piccadilly-face
  '((t (:foreground "white" :background "#000099")))
  "Piccadilly Line Face"
  :group 'org-tfl)

(defface org-tfl-victoria-face
  '((t (:foreground "white" :background "#0099CC")))
  "Victoria Line Face"
  :group 'org-tfl)

(defface org-tfl-waterloo-face
  '((t (:foreground "white" :background "#66CCCC")))
  "Waterloo and City Line Face"
  :group 'org-tfl)

(defvar org-tfl-line-faces
  '(("Bakerloo line" 0 'org-tfl-bakerloo-face prepend)
    ("Central line" 0 'org-tfl-central-face prepend)
    ("Circle line" 0 'org-tfl-circle-face prepend)
    ("District line" 0 'org-tfl-district-face prepend)
    ("Hammersmith & City line" 0 'org-tfl-hammersmith-face prepend)
    ("Jubilee line" 0 'org-tfl-jubliee-face prepend)
    ("Metropolitan line" 0 'org-tfl-metropolitan-face prepend)
    ("Northern line" 0 'org-tfl-northern-face prepend)
    ("Piccadilly line" 0 'org-tfl-piccadilly-face prepend)
    ("Victoria line" 0 'org-tfl-victoria-face prepend)
    ("Waterloo and City line" 0 'org-tfl-waterloo-face prepend))
  "Mapping of lines to faces.")

(defcustom org-tfl-time-format-string "%H:%M"
  "String for 'format-time-string' to display time."
  :type 'string
  :group 'org-tfl)

(defcustom org-tfl-datetime-format-string "%H:%M %d.%m.%Y"
  "String for 'format-time-string' to display date and time."
  :type 'string
  :group 'org-tfl)

(defun org-tfl-get (list &rest keys)
  "Retrieve a value from a LIST with KEYS."
  (let ((list list))
    (while (and list (listp list) keys)
      (setq list (cdr (assoc (car keys) list)))
      (setq keys (cdr keys)))
    (unless keys
      list)))

(defun org-tfl-date-to-time (tfldate)
  "Convert a TFLDATE string of the TFL API to time."
  (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" tfldate)
  (encode-time
   (string-to-number (match-string 6 tfldate))
   (string-to-number (match-string 5 tfldate))
   (string-to-number (match-string 4 tfldate))
   (string-to-number (match-string 3 tfldate))
   (string-to-number (match-string 2 tfldate))
   (string-to-number (match-string 1 tfldate))))

(defun org-tfl-format-date (tfldate)
  "Format the TFLDATE string.

If it's the same day as today, 'org-tfl-time-format-string' is used.
If the date is another day, 'org-tfl-datetime-format-string' is used."
  (let ((time (org-tfl-date-to-time tfldate)))
    (if (zerop (- (time-to-days time)
                  (time-to-days (current-time))))
        (format-time-string org-tfl-time-format-string time)
      (format-time-string org-tfl-datetime-format-string time))))

(defun org-tfl-jp-format-mode-icons (legs)
  "Return a formatted string with the mode icons for LEGS."
  (mapconcat
   (lambda (leg)
     (or (org-tfl-get org-tfl-mode-icons (org-tfl-get leg 'mode 'id))
         (org-tfl-get leg 'mode 'id)))
   legs
   " "))

(defun org-tfl-jp-format-leg-disruption-icon (leg)
  "Return a disruption icon if there are disruptions for the given LEG."
  (if (equal (org-tfl-get leg 'isDisrupted) :json-false)
      ""
    (or
     (cl-loop for disruption across (org-tfl-get leg 'disruptions)
              unless (equal (org-tfl-get disruption 'category) "Information")
              return (concat org-tfl-icon-disruption " "))
     (concat org-tfl-icon-information " "))))

(defun org-tfl-chop (s len)
  "If S is longer than LEN, wrap the words with newlines."
  (with-temp-buffer
    (insert s)
    (let ((fill-column len))
      (fill-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-tfl-jp-format-leg-disruptions (leg level)
  "Return a formatted string with disruptions for the given LEG at 'org-mode' LEVEL."
  (if (equal (cdr (assoc 'isDisrupted leg)) :json-false)
      ""
    (format
     "\n%s %sDisruptions\n%s"
     (make-string level (string-to-char "*"))
     (org-tfl-jp-format-leg-disruption-icon leg)
     (mapconcat
      `(lambda (disruption)
         (format
          "%s %s%s\n%s"
          (make-string ,(+ level 1) (string-to-char "*"))
          (if (equal "Information" (org-tfl-get disruption 'categoryDescription))
              (concat org-tfl-icon-information " ")
            (concat org-tfl-icon-disruption " "))
          (org-tfl-get disruption 'categoryDescription)
          (org-tfl-chop (org-tfl-get disruption 'description) ,fill-column)))
      (org-tfl-get leg 'disruptions)
      "\n"))))

(defun org-tfl-make-maps-url (path)
  "Create a url for the given PATH to a google maps static map."
  (let* ((pathclean (replace-regexp-in-string
                     "\\(\\]\\]\\|\\[\\[\\| \\)" ""
                     path))
         (wplist (split-string pathclean "\\],\\[")))
    (substring
     (cl-loop for start from 0 to (length wplist) by 27 concat
              (format
               "[[org-tfl-map:http://maps.google.com/maps/api/staticmap?size=%sx%s&maptype=%s&path=color:%s|weight:%s|%s&markers=label:S|color:%s|%s&markers=label:E|color:%s|%s][Map%s]]\n"
               org-tfl-map-width
               org-tfl-map-height
               org-tfl-map-type
               org-tfl-map-path-color
               org-tfl-map-path-weight
               (mapconcat 'identity
                          (cl-subseq wplist (max 0 (- start 1)) (min (+ start 26) (length wplist)))
                          "|")
               org-tfl-map-start-marker-color
               (elt wplist (max 0 (- start 1)))
               org-tfl-map-end-marker-color
               (elt wplist (min (+ start 25) (- (length wplist) 1)))
               (+ (/ start 27) 1)))
     0 -1)))

(defun org-tfl-open-map-link (path)
  "Show the map in the buffer."
  (let* ((link (save-match-data (org-element-context)))
         (start (org-element-property :begin link))
         (end (org-element-property :end link))
         (name (concat temporary-file-directory (make-temp-name "orgtflmap") ".png")))
    (url-copy-file path name)
    (delete-region start end)
    (insert (format "[[file:%s]]" name))
    (org-display-inline-images nil t start (+ end 9))))

(org-add-link-type "org-tfl-map" 'org-tfl-open-map-link)

(defun org-tfl-jp-format-steps (leg)
  "Return a formatted string with a list of steps for the given LEG.

The string will be prefixed with a newline character."
  (let ((steps (org-tfl-get leg 'instruction 'steps)))
    (concat
     "\n"
     (org-tfl-make-maps-url (org-tfl-get leg 'path 'lineString))
     (if (> (length steps) 0)
         (concat
          "\n"
          (mapconcat
           `(lambda (step)
              (format
               "- %s"
               (org-tfl-get step 'description)))
           steps
           "\n"))
       ""))))

(defun org-tfl-jp-format-leg-detailed (leg level)
  "Return a detailed formatted string for the given LEG at the given 'org-mode' LEVEL."
  (format
   "%s %s%s%s"
   (make-string level (string-to-char "*"))
   (org-tfl-get leg 'instruction 'detailed)
   (org-tfl-jp-format-steps leg)
   (org-tfl-jp-format-leg-disruptions leg level)))

(defun org-tfl-jp-format-leg (leg level)
  "Return a formatted string for the given LEG at the given 'org-mode' LEVEL."
  (format
   "%s %3smin %s %s %s%s\n%s"
   (make-string level (string-to-char "*"))
   (org-tfl-get leg 'duration)
   (org-tfl-format-date (org-tfl-get leg 'departureTime))
   (or (org-tfl-get org-tfl-mode-icons (org-tfl-get leg 'mode 'id))
       (org-tfl-get leg 'mode 'id))
   (org-tfl-jp-format-leg-disruption-icon leg)
   (org-tfl-get leg 'instruction 'summary)
   (org-tfl-jp-format-leg-detailed leg (+ level 1))))

(defun org-tfl-jp-format-journey-disruption-icon (legs)
  "Return a disruption icon if there are disruptions for the given LEGS."
  (or (cl-loop for leg across legs
               if (equal-including-properties
                   (org-tfl-jp-format-leg-disruption-icon leg)
                   (concat org-tfl-icon-disruption " "))
               return (concat org-tfl-icon-disruption " "))
      ""))

(defun org-tfl-jp-format-journey (journey level)
  "Return a formatted string for the given JOURNEY at the given 'org-mode' LEVEL."
  (let ((legs (org-tfl-get journey 'legs)))
    (format
     "%s %4smin %s %s%s => %s\n%s"
     (make-string level (string-to-char "*"))
     (org-tfl-get journey 'duration)
     (org-tfl-jp-format-mode-icons legs)
     (org-tfl-jp-format-journey-disruption-icon legs)
     (org-tfl-format-date (org-tfl-get journey 'startDateTime))
     (org-tfl-format-date (org-tfl-get journey 'arrivalDateTime))
     (mapconcat `(lambda (leg) (org-tfl-jp-format-leg leg ,(+ level 1)))
                legs
                "\n"))))

(defun org-tfl-jp-format-title (result)
  "Return a formattes string suitable for a title of a Journey Planner RESULT."
  (let ((date (org-tfl-format-date (org-tfl-get result 'searchCriteria 'dateTime)))
        (journey (elt (org-tfl-get result 'journeys) 0)))
    (if journey
        (format
         "%3smin %s %s%s => %s | %s to %s"
         (org-tfl-get journey 'duration)
         (org-tfl-jp-format-mode-icons (org-tfl-get journey 'legs))
         (org-tfl-jp-format-journey-disruption-icon (org-tfl-get journey 'legs))
         (org-tfl-format-date (org-tfl-get journey 'startDateTime))
         (org-tfl-format-date (org-tfl-get journey 'arrivalDateTime))
         (or org-tfl-jp-arg-fromName org-tfl-jp-arg-from)
         (or org-tfl-jp-arg-toName org-tfl-jp-arg-to))
      (format "%s to %s (%s): No journeys found!"
              org-tfl-jp-arg-fromName org-tfl-jp-arg-toName date))))

(defun org-tfl-jp-format-itinerary-result (result level &optional heading)
  "Return a nice formatted string of the given itinerary RESULT.

Heading in the given 'org mode' LEVEL.
No heading if HEADING is nil."
  (concat
   (if heading
       (format
        "%s %s:\n"
        (make-string level (string-to-char "*"))
        (org-tfl-jp-format-title result))
     "")
   (mapconcat
    `(lambda (journey) (org-tfl-jp-format-journey journey ,(+ level 1)))
    (org-tfl-get result 'journeys) "\n")))

(defun org-tfl-jp-itinerary-handler (result resulthandler)
  "Let RESULT be handled by RESULTHANDLER."
  (funcall resulthandler result))

(defun org-tfl-jp-itinerary-show-in-buffer (result)
  "Show itinerary RESULT."
  (let ((journeys (org-tfl-get result 'journeys))
        (level (+ (or (org-current-level) 0) 1)))
    (if (zerop (length journeys))
        (message "No journeys found!")
      (let ((buf (get-buffer-create "Itinerary Results")))
        (display-buffer buf)
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (font-lock-add-keywords nil org-tfl-line-faces t)
          (insert (org-tfl-jp-format-itinerary-result result level t))
          (hide-sublevels (+ level 1)))))))

(defun org-tfl-jp-replace-link (pos desc)
  "Replace the link description at POS with DESC."
  (goto-char pos)
  (let ((linkregion (org-in-regexp org-bracket-link-regexp 1))
        (link (org-link-unescape (org-match-string-no-properties 1)))
        (properties (delete '("FILE") (org-entry-properties pos 'all))))
    (setq properties (delq (assoc "ITEM" properties) properties))
    (add-to-list 'properties (cons "FROM" org-tfl-jp-arg-from))
    (add-to-list 'properties (cons "TO" org-tfl-jp-arg-to))
    (when org-tfl-jp-arg-via
      (add-to-list 'properties (cons "VIA" org-tfl-jp-arg-via)))
    (when org-tfl-jp-arg-fromName
      (add-to-list 'properties (cons "FROMNAME" org-tfl-jp-arg-fromName)))
    (when org-tfl-jp-arg-toName
      (add-to-list 'properties (cons "TONAME" org-tfl-jp-arg-toName)))
    (when org-tfl-jp-arg-viaName
      (add-to-list 'properties (cons "VIANAME" org-tfl-jp-arg-viaName)))
    (delete-region (car linkregion) (cdr linkregion))
    (org-cut-subtree)
    (org-insert-subheading nil)
    (org-promote-subtree)
    (setq org-tfl-org-buffer-point (point))
    (insert (format "[[%s][%s]]\n\n" link desc))
    (goto-char (car linkregion))
    (dolist (prop (reverse properties))
      (org-set-property (car prop) (cdr prop)))))

(defun org-tfl-jp-itinerary-insert-org (result)
  "Insert itinerary RESULT in org mode."
  (let ((journeys (org-tfl-get result 'journeys)))
    (display-buffer org-tfl-org-buffer)
    (with-current-buffer org-tfl-org-buffer
      (font-lock-add-keywords nil org-tfl-line-faces t)
      (org-tfl-jp-replace-link org-tfl-org-buffer-point (org-tfl-jp-format-title result))
      (let* ((level (or (org-current-level) 0))
             (element (org-element-at-point))
             (beginning (org-element-property :contents-begin element)))
        (when beginning
          (goto-char beginning)
          (setq element (org-element-at-point))
          (while (and (or (equal (org-element-type element) 'property-drawer)
                          (equal (org-element-type element) 'drawer)
                          (equal (org-element-type element) 'planning))
                      (not (equal (org-element-property :end element) (point))))
            (when (org-element-property :end element)
              (goto-char (org-element-property :end element))
              (setq element (org-element-at-point)))))
        (unless beginning
          (goto-char (org-element-property :end element)))
        (if (or (equal (org-element-type element) 'property-drawer)
                (equal (org-element-type element) 'drawer)
                (equal (org-element-type element) 'planning))
            (goto-char (- (point) 1))
          (goto-char (- (point) 2)))
        (insert (org-tfl-jp-format-itinerary-result result level nil))
        (goto-char org-tfl-org-buffer-point)
        (hide-subtree)
        (org-cycle)))))

(defun org-tfl-jp-get-disambiguations (result)
  "Set the disambiguation options from RESULT."
  (setq org-tfl-jp-fromdis nil
        org-tfl-jp-todis nil
        org-tfl-jp-viadis nil
        org-tfl-jp-fromdis
        (or (org-tfl-get result 'fromLocationDisambiguation 'disambiguationOptions)
            (equal (org-tfl-get result 'fromLocationDisambiguation 'matchStatus)
                   "identified"))
        org-tfl-jp-todis
        (or (org-tfl-get result 'toLocationDisambiguation 'disambiguationOptions)
            (equal (org-tfl-get result 'toLocationDisambiguation 'matchStatus)
                   "identified"))
        org-tfl-jp-viadis
        (or (org-tfl-get result 'viaLocationDisambiguation 'disambiguationOptions)
            (equal (org-tfl-get result 'viaLocationDisambiguation 'matchStatus)
                   "identified"))))

(defun org-tfl-jp-pp-disambiguation (candidate)
  "Nice formatting for disambiguation CANDIDATE."
  (let* ((place (assoc 'place candidate))
         (type (eval (org-tfl-get place 'placeType)))
         (modes (org-tfl-get place 'modes))
         (commonName (org-tfl-get place 'commonName)))
    (cond ((equal type "StopPoint")
           (if modes
               (concat " " (mapconcat
                            #'(lambda (mode) (or (org-tfl-get org-tfl-mode-icons mode) mode))
                            modes " ")
                       " "
                       commonName)
             (concat " " commonName)))
          ((equal type "PointOfInterest")
           (format " %s %s" org-tfl-icon-cam commonName))
          ((equal type "Address")
           (format " %s %s" org-tfl-icon-location commonName))
          ('t
           (format " %s: %s" type commonName)))))

(defun org-tfl-jp-transform-disambiguations (candidates)
  "Transform disambiguation option CANDIDATES.

Result is a list of (DISPLAY . REAL) values."
  (mapcar (lambda (cand) (cons (org-tfl-jp-pp-disambiguation cand) cand))
          candidates))

(defun org-tfl-jp-resolve-completing-read (cands var commonvar name)
  "Let the user select CANDS to set VAR and COMMONVAR.

NAME for the prompt section."
  (let* ((candstf (org-tfl-jp-transform-disambiguations (eval cands)))
         (option (cdr (assoc
                       (completing-read
                        name
                        candstf
                        nil t)
                       candstf))))
    (setq cands nil)
    (set commonvar (org-tfl-get option 'place 'commonName))
    (set var (format "lonlat:\"%s,%s\""
                     (org-tfl-get option 'place 'lon)
                     (org-tfl-get option 'place 'lat)))))

(defun org-tfl-jp-resolve-disambiguation (resulthandler)
  "Let the user choose from the disambiguation options.

If there are no options retrieve itinerary and call RESULTHANDLER."
  (when (vectorp org-tfl-jp-fromdis)
    (org-tfl-jp-resolve-completing-read
     'org-tfl-jp-fromdis
     'org-tfl-jp-arg-from
     'org-tfl-jp-arg-fromName
     (format "Select FROM location for %s: " org-tfl-jp-arg-from)))
  (when (vectorp org-tfl-jp-todis)
    (org-tfl-jp-resolve-completing-read
     'org-tfl-jp-todis
     'org-tfl-jp-arg-to
     'org-tfl-jp-arg-toName
     (format "Select TO location for %s: " org-tfl-jp-arg-to)))
  (when (vectorp org-tfl-jp-viadis)
    (org-tfl-jp-resolve-completing-read
     'org-tfl-jp-viadis
     'org-tfl-jp-arg-via
     'org-tfl-jp-arg-viaName
     (format "Select VIA location for %s: " org-tfl-jp-arg-via)))
  (url-retrieve
   (org-tfl-jp-make-url)
   `(lambda (status &rest args)
      (apply 'org-tfl-jp-handle ',resulthandler status args))))

(defun org-tfl-jp-disambiguation-handler (result resulthandler)
  "Resolve disambiguation of RESULT and try again with RESULTHANDLER."
  (org-tfl-jp-get-disambiguations result)
  (if (and org-tfl-jp-fromdis org-tfl-jp-todis)
      (org-tfl-jp-resolve-disambiguation resulthandler)
    (if org-tfl-jp-fromdis
        (message "Cannot resolve To Location: %s" org-tfl-jp-arg-to)
      (message "Cannot resolve From Location: %s" org-tfl-jp-arg-from))))

(defvar org-tfl-jp-handlers
  `(("Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-itinerary-handler)
    ("Tfl.Api.Presentation.Entities.JourneyPlanner.DisambiguationResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-disambiguation-handler)))

(defun org-tfl-jp-make-url ()
  "Create journey planner url.

For keys see 'org-tfl-jp-retrieve'."
  (replace-regexp-in-string
   "&+$" ""
   (concat org-tfl-api-base-url
           (format org-tfl-api-jp
                   (url-hexify-string org-tfl-jp-arg-from)
                   (url-hexify-string org-tfl-jp-arg-to))
           "?"
           (if (and org-tfl-api-jp org-tfl-api-key)
               (format "app_id=%s&app_key=%s&" (or org-tfl-api-id "") (or org-tfl-api-key ""))
             "")
           (if org-tfl-jp-arg-via (format "via=%s&" (url-hexify-string org-tfl-jp-arg-via)) "")
           (if org-tfl-jp-arg-nationalSearch
               (format "nationalSearch=%s&" org-tfl-jp-arg-nationalSearch) "")
           (if org-tfl-jp-arg-date (format "date=%s&" org-tfl-jp-arg-date) "")
           (if org-tfl-jp-arg-time (format "time=%s&" org-tfl-jp-arg-time) "")
           (format "timeIs=%s&" org-tfl-jp-arg-timeIs)
           (format "journeyPreference=%s&" org-tfl-jp-arg-journeyPreference)
           (if org-tfl-jp-arg-mode (format "mode=%s&" org-tfl-jp-arg-mode) "")
           (if org-tfl-jp-arg-accessibilityPreference (format "accessibilityPreference=%s&"
                                                              org-tfl-jp-arg-accessibilityPreference) "")
           (if org-tfl-jp-arg-fromName
               (format "fromName=%s&" (url-hexify-string org-tfl-jp-arg-fromName)) "")
           (if org-tfl-jp-arg-toName
               (format "toName=%s&" (url-hexify-string org-tfl-jp-arg-toName)) "")
           (if org-tfl-jp-arg-viaName
               (format "viaName=%s&" (url-hexify-string org-tfl-jp-arg-viaName)) "")
           (if org-tfl-jp-arg-maxTransferMinutes
               (format "maxTransferMinutes=%s&" org-tfl-jp-arg-maxTransferMinutes) "")
           (if org-tfl-jp-arg-maxWalkingMinutes
               (format "maxWalkingMinutes=%s&" org-tfl-jp-arg-maxWalkingMinutes) "")
           (format "average=%s&" org-tfl-jp-arg-walkingSpeed)
           (if org-tfl-jp-arg-cyclePreference
               (format "cyclePreference=%s&" org-tfl-jp-arg-cyclePreference) "")
           (if org-tfl-jp-arg-adjustment (format "adjustment=%s&" org-tfl-jp-arg-adjustment) "")
           (if org-tfl-jp-arg-bikeProficiency
               (format "bikeProficiency=%s&" org-tfl-jp-arg-bikeProficiency) "")
           (if org-tfl-jp-arg-alternativeCycle
               (format "alternativeCycle=%s&" org-tfl-jp-arg-alternativeCycle) "")
           (if org-tfl-jp-arg-alternativeWalking
               (format "alternativeWalking=%s&" org-tfl-jp-arg-alternativeWalking) "")
           (if org-tfl-jp-arg-applyHtmlMarkup
               (format "applyHtmlMarkup=%s&" org-tfl-jp-arg-applyHtmlMarkup) "")
           (if org-tfl-jp-arg-useMultiModalCall
               (format "useMultiModalCall=%s&" org-tfl-jp-arg-useMultiModalCall) ""))))

(defun org-tfl-jp-handle-error (data response)
  "Handle errors with DATA and RESPONSE."
  (if (eq (nth 1 (car data)) 'http)
      (message "HTTP %s: %s" (nth 2 (car data)) response)))

(defun org-tfl-jp-handle-redirect (data response)
  "Handle redirect errors with DATA and RESPONSE."
  (message "Got redirected. Are you sure you supplied the correct credentials?"))

(defun org-tfl-jp-handle (resulthandler status &rest args)
  "Handle the result of a jp request with RESULTHANDLER.

If status is not successful other handlers are called STATUS.
ARGS are ignored."
  (goto-char url-http-end-of-headers)
  (cond ((eq (car status) :error)
         (org-tfl-jp-handle-error (cdr status) (buffer-substring (point) (point-max))))
        ((eq (car status) :redirect)
         (org-tfl-jp-handle-redirect (cdr status) (buffer-substring (point) (point-max))))
        (t
         (let* ((result (json-read))
                (type (org-tfl-get result '$type))
                (handler (org-tfl-get org-tfl-jp-handlers type)))
           (funcall handler result resulthandler)))))

(cl-defun org-tfl-jp-retrieve
    (from to &key
          (via nil) (nationalSearch nil) (date nil) (time nil) (timeIs "Departing")
          (journeyPreference "leasttime") (mode nil) (accessibilityPreference nil)
          (fromName nil) (toName nil) (viaName nil) (maxTransferMinutes nil)
          (maxWalkingMinutes nil) (walkingSpeed "average") (cyclePreference nil)
          (adjustment nil) (bikeProficiency nil) (alternativeCycle nil)
          (alternativeWalking nil) (applyHtmlMarkup nil) (useMultiModalCall nil)
          (resulthandler 'org-tfl-jp-itinerary-show-in-buffer))
  "Retrieve journey result FROM TO with PARAMS.

FROM and TO are locations and can be names, Stop-IDs or coordinates of the format
\"lonlat:0.12345,67.890\".
VIA can be an optional place between FROM and TO.
NATIONALSEARCH should be \"True\" for journeys outside London.
DATE of the journey in yyyyMMdd format.
TIME of the journey in HHmm format.
TIMEIS does the given DATE and TIME relate to departure or arrival, e.g.
\"Departing\" | \"Arriving\".
JOURNEYPREFERENCE \"leastinterchange\" | \"leasttime\" | \"leastwalking\".
MODE comma seperated list, possible options \"black-cab-as-customer,black-cab-as-driver,bus,cable-car,coach,cycle,cycle-hire,dlr,electric-car,goods-vehicle-as-driver,interchange-keep-sitting,interchange-secure,international-rail,motorbike-scooter,national-rail,overground,plane,private-car,private-coach-as-customer,private-coach-as-driver,private-hire-as-customer,private-hire-as-driver,replacement-bus,river-bus,river-tour,tflrail,tram,tube,walking\".
ACCESSIBILITYPREFERENCE comma seperated list, possible options \"noSolidStairs,noEscalators,noElevators,stepFreeToVehicle,stepFreeToPlatform\".
FROMNAME is the location name associated with a from coordinate.
TONAME is the location name associated with a to coordinate.
VIANAME is the location name associated with a via coordinate.
MAXTRANSFERMINUTES The max walking time in minutes for transfer eg. \"120\".
MAXWALKINGMINUTES The max walking time in minutes for journey eg. \"120\".
WALKINGSPEED \"slow\" | \"average\" | \"fast\".
CYCLEPREFERENCE \"allTheWay\" | \"leaveAtStation\" | \"takeOnTransport\" | \"cycleHire\".
ADJUSTMENT time adjustment command, e.g. \"TripFirst\" | \"TripLast\".
BIKEPROFICIENCY comma seperated list, possible options \"easy,moderate,fast\".
ALTERNATIVECYCLE Option to determine whether to return alternative cycling journey.
ALTERNATIVEWALKING Option to determine whether to return alternative walking journey.
APPLYHTMLMARKUP Flag to determine whether certain text (e.g. walking instructions) should be output with HTML tags or not.
USEMULTIMODALCALL A boolean to indicate whether or not to return 3 public transport journeys, a bus journey, a cycle hire journey, a personal cycle journey and a walking journey.
RESULTHANDLER is the function to call after retrieving the result."
  (setq org-tfl-jp-arg-from from
        org-tfl-jp-arg-to to
        org-tfl-jp-arg-via via
        org-tfl-jp-arg-fromName from
        org-tfl-jp-arg-toName to
        org-tfl-jp-arg-viaName via
        org-tfl-jp-arg-nationalSearch nationalSearch
        org-tfl-jp-arg-date date
        org-tfl-jp-arg-time time
        org-tfl-jp-arg-timeIs timeIs
        org-tfl-jp-arg-journeyPreference journeyPreference
        org-tfl-jp-arg-mode mode
        org-tfl-jp-arg-accessibilityPreference accessibilityPreference
        org-tfl-jp-arg-fromName fromName
        org-tfl-jp-arg-toName toName
        org-tfl-jp-arg-viaName viaName
        org-tfl-jp-arg-maxTransferMinutes maxTransferMinutes
        org-tfl-jp-arg-maxWalkingMinutes maxWalkingMinutes
        org-tfl-jp-arg-walkingSpeed walkingSpeed
        org-tfl-jp-arg-cyclePreference cyclePreference
        org-tfl-jp-arg-adjustment adjustment
        org-tfl-jp-arg-bikeProficiency bikeProficiency
        org-tfl-jp-arg-alternativeCycle alternativeCycle
        org-tfl-jp-arg-alternativeWalking alternativeWalking
        org-tfl-jp-arg-applyHtmlMarkup applyHtmlMarkup
        org-tfl-jp-arg-useMultiModalCall useMultiModalCall

        org-tfl-jp-fromdis nil
        org-tfl-jp-todis nil
        org-tfl-jp-viadis nil)

  (url-retrieve
   (org-tfl-jp-make-url)
   `(lambda (status &rest args)
      (apply 'org-tfl-jp-handle ',resulthandler status args))))

;;;###autoload
(defun org-tfl-jp (from to via datetime timeIs)
  "Plan journey FROM TO VIA at DATETIME.

TIMEIS if t, DATETIME is the departing time."
  (interactive
   (list (read-from-minibuffer "From: " nil nil nil 'org-tfl-from-history)
         (read-from-minibuffer "To: " nil nil nil 'org-tfl-to-history)
         (read-from-minibuffer "Via: " nil nil nil 'org-tfl-via-history)
         (org-read-date t t)
         (yes-or-no-p "Time is departure time? No for arrival time:")))
  (let ((date (format-time-string "%Y%m%d" datetime))
        (time (format-time-string "%H%M" datetime))
        (timeis (if timeIs "Departing" "Arriving")))
    (org-tfl-jp-retrieve from to
                         :via (if (equal "" via) nil via)
                         :date date :time time :timeIs timeis)))

(cl-defun org-tfl-jp-retrieve-org (from to &rest keywords &allow-other-keys)
  "Use 'org-tfl-jp-itinerary-insert-org' as handlefunc.

Inserts the result in the current buffer.
For the rest see 'org-tfl-jp-retrieve'."
  (setq org-tfl-org-buffer (current-buffer))
  (setq org-tfl-org-buffer-point (point))
  (apply 'org-tfl-jp-retrieve from to :resulthandler 'org-tfl-jp-itinerary-insert-org keywords))

(defun org-tfl-jp-open-org-link (&optional path)
  "Open a org-tfl link.  PATH is ignored.  Properties of the paragraph are used instead."
  (let* ((element (org-element-at-point))
         (FROM (org-element-property :FROM element))
         (TO (org-element-property :TO element))
         (VIA (org-element-property :VIA element))
         (NATIONALSEARCH (org-element-property :NATIONALSEARCH element))
         (SCHEDULED (org-get-scheduled-time (point)))
         (DATE nil)
         (TIME nil)
         (TIMEIS (or (org-element-property :TIMEIS element) "Departing"))
         (JOURNEYPREFERENCE (or (org-element-property :JOURNEYPREFERENCE element) "leasttime"))
         (MODE (org-element-property :MODE element))
         (ACCESSIBILITYPREFERENCE (org-element-property :ACCESSIBILITYPREFERENCE element))
         (FROMNAME (org-element-property :FROMNAME element))
         (TONAME (org-element-property :TONAME element))
         (VIANAME (org-element-property :VIANAME element))
         (MAXTRANSFERMINUTES (org-element-property :MAXTRANSFERMINUTE element))
         (MAXWALKINGMINUTES (org-element-property :MAXWALKINMINUTES element))
         (WALKINGSPEED (or (org-element-property :WALKINGSPEED element) "average"))
         (CYCLEPREFERENCE (org-element-property :CYCLEPREFERENCE element))
         (ADJUSTMENT (org-element-property :ADJUSTMENT element))
         (BIKEPROFICIENCY (org-element-property :BIKEPROFICIENCY element))
         (ALTERNATIVECYCLE (org-element-property :ALTERNATIVECYCLE element))
         (ALTERNATIVEWALKING (org-element-property :ALTERNATIVEWALKING element)))
    (when SCHEDULED
      (setq DATE (format-time-string "%Y%m%d" SCHEDULED))
      (setq TIME (format-time-string "%H%M" SCHEDULED)))
    (org-tfl-jp-retrieve-org
     FROM TO :via VIA :nationalSearch NATIONALSEARCH :date DATE :time TIME
     :timeIs TIMEIS :journeyPreference JOURNEYPREFERENCE :mode MODE
     :accessibilityPreference ACCESSIBILITYPREFERENCE :fromName FROMNAME
     :toName TONAME :viaName VIANAME :maxTransferMinutes MAXTRANSFERMINUTES
     :maxWalkingMinutes MAXWALKINGMINUTES :walkingSpeed WALKINGSPEED
     :cyclePreference CYCLEPREFERENCE :adjustment ADJUSTMENT :bikeProficiency BIKEPROFICIENCY
     :alternativeCycle ALTERNATIVECYCLE :alternativeWalking ALTERNATIVEWALKING)))

(org-add-link-type "org-tfl" 'org-tfl-jp-open-org-link)

;;;###autoload
(defun org-tfl-jp-org (from to via datetime timeIs)
  "Plan journey FROM TO VIA at DATETIME.

This creates a subheading and a link to update the result.
The leave of the subheading is the journey result.
TIMEIS if t, DATETIME is the departing time."
  (interactive
   (list (read-from-minibuffer "From: " nil nil nil 'org-tfl-from-history)
         (read-from-minibuffer "To: " nil nil nil 'org-tfl-to-history)
         (read-from-minibuffer "Via: " nil nil nil 'org-tfl-via-history)
         (org-read-date t t)
         (yes-or-no-p "Time is departure time? No for arrival time:")))
  (let ((timeis (if timeIs "Departing" "Arriving")))
    (org-insert-subheading nil)
    (org-promote)
    (insert "[[org-tfl:][Retrieving Information...]]")
    (org-set-property "FROM" from)
    (org-set-property "TO" to)
    (unless (equal via "")
      (org-set-property "VIA" via))
    (org-schedule nil (format-time-string (cdr org-time-stamp-formats) datetime))
    (if timeIs
        (org-set-property "TIMEIS" "Departing")
      (org-set-property "TIMEIS" "Arriving"))
    (org-tfl-jp-open-org-link)))

;; FIX 300 status code for disambiguation result to set success to t.
;; If we do not do this, the TfL API will return a 300 status code and
;; url-retrieve will get stuck in "Spinning waiting for headers", which never
;; completes so the callback is never called.
;; Curiously it did work at first but after an update to
;; the most recent versions of all packages, it doesn't (or did from time to time).
(defun url-http-parse-headers ()
  "Parse and handle HTTP specific headers.
Return t if and only if the current buffer is still active and
should be shown to the user."
  ;; The comments after each status code handled are taken from RFC
  ;; 2616 (HTTP/1.1)
  (url-http-mark-connection-as-free (url-host url-current-object)
                                    (url-port url-current-object)
                                    url-http-process)

  (if (or (not (boundp 'url-http-end-of-headers))
          (not url-http-end-of-headers))
      (error "Trying to parse headers in odd buffer: %s" (buffer-name)))
  (goto-char (point-min))
  (url-http-debug "url-http-parse-headers called in (%s)" (buffer-name))
  (url-http-parse-response)
  (mail-narrow-to-head)
  ;;(narrow-to-region (point-min) url-http-end-of-headers)
  (let ((connection (mail-fetch-field "Connection")))
    ;; In HTTP 1.0, keep the connection only if there is a
    ;; "Connection: keep-alive" header.
    ;; In HTTP 1.1 (and greater), keep the connection unless there is a
    ;; "Connection: close" header
    (cond
     ((string= url-http-response-version "1.0")
      (unless (and connection
                   (string= (downcase connection) "keep-alive"))
        (delete-process url-http-process)))
     (t
      (when (and connection
                 (string= (downcase connection) "close"))
        (delete-process url-http-process)))))
  (let* ((buffer (current-buffer))
         (class (/ url-http-response-status 100))
         (success nil)
         ;; other status symbols: jewelry and luxury cars
         (status-symbol (cadr (assq url-http-response-status url-http-codes))))
    (url-http-debug "Parsed HTTP headers: class=%d status=%d"
                    class url-http-response-status)
    (when (url-use-cookies url-http-target-url)
      (url-http-handle-cookies))

    (pcase class
      ;; Classes of response codes
      ;;
      ;; 5xx = Server Error
      ;; 4xx = Client Error
      ;; 3xx = Redirection
      ;; 2xx = Successful
      ;; 1xx = Informational
      (1				; Information messages
       ;; 100 = Continue with request
       ;; 101 = Switching protocols
       ;; 102 = Processing (Added by DAV)
       (url-mark-buffer-as-dead buffer)
       (error "HTTP responses in class 1xx not supported (%d)"
              url-http-response-status))
      (2				; Success
       ;; 200 Ok
       ;; 201 Created
       ;; 202 Accepted
       ;; 203 Non-authoritative information
       ;; 204 No content
       ;; 205 Reset content
       ;; 206 Partial content
       ;; 207 Multi-status (Added by DAV)
       (pcase status-symbol
         ((or `no-content `reset-content)
          ;; No new data, just stay at the same document
          (url-mark-buffer-as-dead buffer))
         (_
          ;; Generic success for all others.  Store in the cache, and
          ;; mark it as successful.
          (widen)
          (if (and url-automatic-caching (equal url-http-method "GET"))
              (url-store-in-cache buffer))))
       (setq success t))
      (3				; Redirection
       ;; 300 Multiple choices
       ;; 301 Moved permanently
       ;; 302 Found
       ;; 303 See other
       ;; 304 Not modified
       ;; 305 Use proxy
       ;; 307 Temporary redirect
       (let ((redirect-uri (or (mail-fetch-field "Location")
                               (mail-fetch-field "URI"))))
         (pcase status-symbol
           (`multiple-choices	    ; 300
            ;; Quoth the spec (section 10.3.1)
            ;; -------------------------------
            ;; The requested resource corresponds to any one of a set of
            ;; representations, each with its own specific location and
            ;; agent-driven negotiation information is being provided so
            ;; that the user can select a preferred representation and
            ;; redirect its request to that location.
            ;; [...]
            ;; If the server has a preferred choice of representation, it
            ;; SHOULD include the specific URI for that representation in
            ;; the Location field; user agents MAY use the Location field
            ;; value for automatic redirection.
            ;; -------------------------------
            ;; We do not support agent-driven negotiation, so we just
            ;; redirect to the preferred URI if one is provided.
            (setq success t))
           ((or `moved-permanently `found `temporary-redirect) ; 301 302 307
            ;; If the 301|302 status code is received in response to a
            ;; request other than GET or HEAD, the user agent MUST NOT
            ;; automatically redirect the request unless it can be
            ;; confirmed by the user, since this might change the
            ;; conditions under which the request was issued.
            (unless (member url-http-method '("HEAD" "GET"))
              (setq redirect-uri nil)))
           (`see-other			; 303
            ;; The response to the request can be found under a different
            ;; URI and SHOULD be retrieved using a GET method on that
            ;; resource.
            (setq url-http-method "GET"
                  url-http-data nil))
           (`not-modified		; 304
            ;; The 304 response MUST NOT contain a message-body.
            (url-http-debug "Extracting document from cache... (%s)"
                            (url-cache-create-filename (url-view-url t)))
            (url-cache-extract (url-cache-create-filename (url-view-url t)))
            (setq redirect-uri nil
                  success t))
           (`use-proxy			; 305
            ;; The requested resource MUST be accessed through the
            ;; proxy given by the Location field.  The Location field
            ;; gives the URI of the proxy.  The recipient is expected
            ;; to repeat this single request via the proxy.  305
            ;; responses MUST only be generated by origin servers.
            (error "Redirection thru a proxy server not supported: %s"
                   redirect-uri))
           (_
            ;; Treat everything like '300'
            nil))
         (when redirect-uri
           ;; Clean off any whitespace and/or <...> cruft.
           (if (string-match "\\([^ \t]+\\)[ \t]" redirect-uri)
               (setq redirect-uri (match-string 1 redirect-uri)))
           (if (string-match "^<\\(.*\\)>$" redirect-uri)
               (setq redirect-uri (match-string 1 redirect-uri)))

           ;; Some stupid sites (like sourceforge) send a
           ;; non-fully-qualified URL (ie: /), which royally confuses
           ;; the URL library.
           (if (not (string-match url-nonrelative-link redirect-uri))
               ;; Be careful to use the real target URL, otherwise we may
               ;; compute the redirection relative to the URL of the proxy.
               (setq redirect-uri
                     (url-expand-file-name redirect-uri url-http-target-url)))
           (let ((url-request-method url-http-method)
                 (url-request-data url-http-data)
                 (url-request-extra-headers url-http-extra-headers))
             ;; Check existing number of redirects
             (if (or (< url-max-redirections 0)
                     (and (> url-max-redirections 0)
                          (let ((events (car url-callback-arguments))
                                (old-redirects 0))
                            (while events
                              (if (eq (car events) :redirect)
                                  (setq old-redirects (1+ old-redirects)))
                              (and (setq events (cdr events))
                                   (setq events (cdr events))))
                            (< old-redirects url-max-redirections))))
                 ;; url-max-redirections hasn't been reached, so go
                 ;; ahead and redirect.
                 (progn
                   ;; Remember that the request was redirected.
                   (setf (car url-callback-arguments)
                         (nconc (list :redirect redirect-uri)
                                (car url-callback-arguments)))
                   ;; Put in the current buffer a forwarding pointer to the new
                   ;; destination buffer.
                   ;; FIXME: This is a hack to fix url-retrieve-synchronously
                   ;; without changing the API.  Instead url-retrieve should
                   ;; either simply not return the "destination" buffer, or it
                   ;; should take an optional `dest-buf' argument.
                   (set (make-local-variable 'url-redirect-buffer)
                        (url-retrieve-internal
                         redirect-uri url-callback-function
                         url-callback-arguments
                         (url-silent url-current-object)
                         (not (url-use-cookies url-current-object))))
                   (url-mark-buffer-as-dead buffer))
               ;; We hit url-max-redirections, so issue an error and
               ;; stop redirecting.
               (url-http-debug "Maximum redirections reached")
               (setf (car url-callback-arguments)
                     (nconc (list :error (list 'error 'http-redirect-limit
                                               redirect-uri))
                            (car url-callback-arguments)))
               (setq success t))))))
      (4				; Client error
       ;; 400 Bad Request
       ;; 401 Unauthorized
       ;; 402 Payment required
       ;; 403 Forbidden
       ;; 404 Not found
       ;; 405 Method not allowed
       ;; 406 Not acceptable
       ;; 407 Proxy authentication required
       ;; 408 Request time-out
       ;; 409 Conflict
       ;; 410 Gone
       ;; 411 Length required
       ;; 412 Precondition failed
       ;; 413 Request entity too large
       ;; 414 Request-URI too large
       ;; 415 Unsupported media type
       ;; 416 Requested range not satisfiable
       ;; 417 Expectation failed
       ;; 422 Unprocessable Entity (Added by DAV)
       ;; 423 Locked
       ;; 424 Failed Dependency
       (setq success
             (pcase status-symbol
               (`unauthorized			; 401
                ;; The request requires user authentication.  The response
                ;; MUST include a WWW-Authenticate header field containing a
                ;; challenge applicable to the requested resource.  The
                ;; client MAY repeat the request with a suitable
                ;; Authorization header field.
                (url-http-handle-authentication nil))
               (`payment-required              ; 402
                ;; This code is reserved for future use
                (url-mark-buffer-as-dead buffer)
                (error "Somebody wants you to give them money"))
               (`forbidden			; 403
                ;; The server understood the request, but is refusing to
                ;; fulfill it.  Authorization will not help and the request
                ;; SHOULD NOT be repeated.
                t)
               (`not-found			; 404
                ;; Not found
                t)
               (`method-not-allowed		; 405
                ;; The method specified in the Request-Line is not allowed
                ;; for the resource identified by the Request-URI.  The
                ;; response MUST include an Allow header containing a list of
                ;; valid methods for the requested resource.
                t)
               (`not-acceptable		; 406
                ;; The resource identified by the request is only capable of
                ;; generating response entities which have content
                ;; characteristics not acceptable according to the accept
                ;; headers sent in the request.
                t)
               (`proxy-authentication-required ; 407
                ;; This code is similar to 401 (Unauthorized), but indicates
                ;; that the client must first authenticate itself with the
                ;; proxy.  The proxy MUST return a Proxy-Authenticate header
                ;; field containing a challenge applicable to the proxy for
                ;; the requested resource.
                (url-http-handle-authentication t))
               (`request-timeout		; 408
                ;; The client did not produce a request within the time that
                ;; the server was prepared to wait.  The client MAY repeat
                ;; the request without modifications at any later time.
                t)
               (`conflict			; 409
                ;; The request could not be completed due to a conflict with
                ;; the current state of the resource.  This code is only
                ;; allowed in situations where it is expected that the user
                ;; might be able to resolve the conflict and resubmit the
                ;; request.  The response body SHOULD include enough
                ;; information for the user to recognize the source of the
                ;; conflict.
                t)
               (`gone                          ; 410
                ;; The requested resource is no longer available at the
                ;; server and no forwarding address is known.
                t)
               (`length-required		; 411
                ;; The server refuses to accept the request without a defined
                ;; Content-Length.  The client MAY repeat the request if it
                ;; adds a valid Content-Length header field containing the
                ;; length of the message-body in the request message.
                ;;
                ;; NOTE - this will never happen because
                ;; `url-http-create-request' automatically calculates the
                ;; content-length.
                t)
               (`precondition-failed		; 412
                ;; The precondition given in one or more of the
                ;; request-header fields evaluated to false when it was
                ;; tested on the server.
                t)
               ((or `request-entity-too-large `request-uri-too-large) ; 413 414
                ;; The server is refusing to process a request because the
                ;; request entity|URI is larger than the server is willing or
                ;; able to process.
                t)
               (`unsupported-media-type	; 415
                ;; The server is refusing to service the request because the
                ;; entity of the request is in a format not supported by the
                ;; requested resource for the requested method.
                t)
               (`requested-range-not-satisfiable ; 416
                ;; A server SHOULD return a response with this status code if
                ;; a request included a Range request-header field, and none
                ;; of the range-specifier values in this field overlap the
                ;; current extent of the selected resource, and the request
                ;; did not include an If-Range request-header field.
                t)
               (`expectation-failed		; 417
                ;; The expectation given in an Expect request-header field
                ;; could not be met by this server, or, if the server is a
                ;; proxy, the server has unambiguous evidence that the
                ;; request could not be met by the next-hop server.
                t)
               (_
                ;; The request could not be understood by the server due to
                ;; malformed syntax.  The client SHOULD NOT repeat the
                ;; request without modifications.
                t)))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
         (setf (car url-callback-arguments)
               (nconc (list :error (list 'error 'http url-http-response-status))
                      (car url-callback-arguments)))))
      (5
       ;; 500 Internal server error
       ;; 501 Not implemented
       ;; 502 Bad gateway
       ;; 503 Service unavailable
       ;; 504 Gateway time-out
       ;; 505 HTTP version not supported
       ;; 507 Insufficient storage
       (setq success t)
       (pcase url-http-response-status
         (`not-implemented		; 501
          ;; The server does not support the functionality required to
          ;; fulfill the request.
          nil)
         (`bad-gateway			; 502
          ;; The server, while acting as a gateway or proxy, received
          ;; an invalid response from the upstream server it accessed
          ;; in attempting to fulfill the request.
          nil)
         (`service-unavailable		; 503
          ;; The server is currently unable to handle the request due
          ;; to a temporary overloading or maintenance of the server.
          ;; The implication is that this is a temporary condition
          ;; which will be alleviated after some delay.  If known, the
          ;; length of the delay MAY be indicated in a Retry-After
          ;; header.  If no Retry-After is given, the client SHOULD
          ;; handle the response as it would for a 500 response.
          nil)
         (`gateway-timeout		; 504
          ;; The server, while acting as a gateway or proxy, did not
          ;; receive a timely response from the upstream server
          ;; specified by the URI (e.g. HTTP, FTP, LDAP) or some other
          ;; auxiliary server (e.g. DNS) it needed to access in
          ;; attempting to complete the request.
          nil)
         (`http-version-not-supported	; 505
          ;; The server does not support, or refuses to support, the
          ;; HTTP protocol version that was used in the request
          ;; message.
          nil)
         (`insufficient-storage		; 507 (DAV)
          ;; The method could not be performed on the resource
          ;; because the server is unable to store the representation
          ;; needed to successfully complete the request.  This
          ;; condition is considered to be temporary.  If the request
          ;; which received this status code was the result of a user
          ;; action, the request MUST NOT be repeated until it is
          ;; requested by a separate user action.
          nil))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
         (setf (car url-callback-arguments)
               (nconc (list :error (list 'error 'http url-http-response-status))
                      (car url-callback-arguments)))))
      (_
       (error "Unknown class of HTTP response code: %d (%d)"
              class url-http-response-status)))
    (if (not success)
        (url-mark-buffer-as-dead buffer)
      (url-handle-content-transfer-encoding))
    (url-http-debug "Finished parsing HTTP headers: %S" success)
    (widen)
    (goto-char (point-min))
    success))

(provide 'org-tfl)
;;; org-tfl.el ends here
