;;; org-tfl --- Transport for London meets Orgmode

;;; Commentary:

;;; Code:
(require 'url)
(require 'json)

(defvar url-http-end-of-headers nil)
(defvar org-tfl-api-id "f9af66c5")
(defvar org-tfl-api-key nil)
(defvar org-tfl-api-base-url "https://api.tfl.gov.uk/")
(defvar org-tfl-api-jp "Journey/JourneyResults/%s/to/%s")

(defun org-tfl-jp-itinerary-handler (result)
  (display-buffer (current-buffer))
    result)

(defun org-tfl-jp-disambiguation-handler (result)
  (display-buffer (current-buffer))
    result)

(defvar org-tfl-jp-handlers
  `(("Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-itinerary-handler)
    ("Tfl.Api.Presentation.Entities.JourneyPlanner.DisambiguationResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-disambiguation-handler)))

(defun org-tfl-jp-make-url (from to params)
  "Create journey planner url FROM TO with PARAMS."
  (concat org-tfl-api-base-url
	  (format org-tfl-api-jp from to)
	  (if (and org-tfl-api-jp org-tfl-api-key)
	      (format "?app_id=%s&app_key=%s&" (or org-tfl-api-id "") (or org-tfl-api-key ""))
	    "")
	  params))

(defun org-tfl-jp-handle-error (data response)
  "Handle errors with DATA and RESPONSE."
  (if (eq (nth 1 (car data)) 'http)
      (message "HTTP %s: %s" (nth 2 (car data)) response)))

(defun org-tfl-jp-handle-redirect (data response)
  "Handle redirect errors with DATA and RESPONSE."
  (message "Got redirected. Are you sure you supplied the correct credentials?"))

(defun org-tfl-jp-handle (status &rest args)
  "Handle the result of a jp request with STATUS and optional ARGS.

ARGS are ignored."
  (goto-char url-http-end-of-headers)
  (cond ((eq (car status) :error)
	 (org-tfl-jp-handle-error (cdr status) (buffer-substring (point) (point-max))))
	((eq (car status) :redirect)
	 (org-tfl-jp-handle-redirect (cdr status) (buffer-substring (point) (point-max))))
	(t
	 (let* ((result (json-read))
		(type (alist-get '$type result))
		(handler (cdr (assoc type org-tfl-jp-handlers))))
	   (funcall handler result)))))

(defun org-tfl-jp-retrieve (from to params)
  "Retrieve journey result FROM TO with PARAMS."
  (url-retrieve (org-tfl-jp-make-url from to params) 'org-tfl-jp-handle))


;; Example calls
;; (org-tfl-jp-retrieve "lonlat:\"-0.13500003041,51.50990587838\"" "lonlat:\"-0.29547881328,51.57205666482\"" "nationalSearch=False&timeIs=Departing&journeyPreference=LeastTime&walkingSpeed=Average&cyclePreference=None&alternativeCycle=False&alternativeWalking=True&applyHtmlMarkup=False&useMultiModalCall=False")

;; (org-tfl-jp-retrieve "Piccadilly Circus" "Preston Road" "nationalSearch=False&timeIs=Departing&journeyPreference=LeastTime&walkingSpeed=Average&cyclePreference=None&alternativeCycle=False&alternativeWalking=True&applyHtmlMarkup=False&useMultiModalCall=False")


(provide 'org-tfl)
;;; org-tfl ends here
