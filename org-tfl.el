;;; org-tfl --- Transport for London meets Orgmode

;;; Commentary:

;;; Code:
(require 'url)
(require 'json)

(defvar url-http-end-of-headers nil)
(defvar org-tfl-api-id "f9af66c5")
(defvar org-tfl-api-base-url "https://api.tfl.gov.uk/")
(defvar org-tfl-api-jp "Journey/JourneyResults/%s/to/%s")

(defun org-tfl-jp-itinerary-handler (result)
    result)

(defun org-tfl-jp-disambiguation-handler (result)
    result)

(defvar org-tfl-jp-handlers
  `(("Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-itinerary-handler)
    ("Tfl.Api.Presentation.Entities.JourneyPlanner.DisambiguationResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-disambiguation-handler)))

(defun org-tfl-jp (from to params)
  "Retrieve journey result FROM TO with PARAMS."
  (with-current-buffer
      (url-retrieve-synchronously (concat org-tfl-api-base-url (format org-tfl-api-jp from to)) (format "?app_id=%s&" org-tfl-api-id) params)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun org-tfl-jp-handle (from to params)
  "Handle different result types for the journey request FROM TO with PARAMS."
  (let* ((result (org-tfl-jp from to params))
	 (type (alist-get '$type result))
	 (handler (cdr (assoc type org-tfl-jp-handlers))))
    (funcall handler result)
  ))

;; Example calls
;; (org-tfl-jp-handle "lonlat:\"-0.133794,51.510055\"" "lonlat:\"-0.295107,51.571979\"" "nationalSearch=False&timeIs=Departing&journeyPreference=LeastTime&walkingSpeed=Average&cyclePreference=None&alternativeCycle=False&alternativeWalking=True&applyHtmlMarkup=False&useMultiModalCall=False&&app_key=")

;; (org-tfl-jp-handle "Piccadilly Circus" "Preston Road" "nationalSearch=False&timeIs=Departing&journeyPreference=LeastTime&walkingSpeed=Average&cyclePreference=None&alternativeCycle=False&alternativeWalking=True&applyHtmlMarkup=False&useMultiModalCall=False&&app_key=")


(provide 'org-tfl)
;;; org-tfl ends here
