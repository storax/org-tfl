;;; org-tfl --- Transport for London meets Orgmode

;;; Commentary:

;;; Code:
(require 'url)
(require 'json)
(require 'cl-lib)


(defvar url-http-end-of-headers nil)
(defvar org-tfl-api-id "f9af66c5")
(defvar org-tfl-api-key nil)
(defvar org-tfl-api-base-url "https://api.tfl.gov.uk/")
(defvar org-tfl-api-jp "Journey/JourneyResults/%s/to/%s")

;; JP context
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

(defvar org-tfl-jp-fromdis nil)
(defvar org-tfl-jp-todis nil)
(defvar org-tfl-jp-viadis nil)


(defun org-tfl-jp-itinerary-handler (result)
  "Show itinerary RESULT."
  (display-buffer (current-buffer))
    result)

(defun org-tfl-jp-disambiguation-handler (result)
  "Resolve disambiguation of RESULT and try again."
  (let ((disambiguations (org-tfl-jp-get-disambiguations result)))
    (setq org-tfl-jp-fromdis (nth 0 disambiguations))
    (setq org-tfl-jp-todis (nth 1 disambiguations))
    (setq org-tfl-jp-viadis (nth 2 disambiguations)))
  (display-buffer (current-buffer))
    result)

(defvar org-tfl-jp-handlers
  `(("Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-itinerary-handler)
    ("Tfl.Api.Presentation.Entities.JourneyPlanner.DisambiguationResult, Tfl.Api.Presentation.Entities"
     . org-tfl-jp-disambiguation-handler)))

(defun org-tfl-jp-make-url ()
  "Create journey planner url.

For keys see 'org-tfl-jp-retrieve'."
  (concat org-tfl-api-base-url
	  (format org-tfl-api-jp org-tfl-jp-arg-from org-tfl-jp-arg-to)
	  "?"
	  (if (and org-tfl-api-jp org-tfl-api-key)
	      (format "app_id=%s&app_key=%s&" (or org-tfl-api-id "") (or org-tfl-api-key ""))
	    "")
	  (if org-tfl-jp-arg-via (format "via=%s&" org-tfl-jp-arg-via) "")
	  (if org-tfl-jp-arg-nationalSearch
	      (format "nationalSearch=%s&" org-tfl-jp-arg-nationalSearch) "")
	  (if org-tfl-jp-arg-date (format "date=%s&" org-tfl-jp-arg-date) "")
	  (if org-tfl-jp-arg-time (format "time=%s&" org-tfl-jp-arg-time) "")
	  (format "timeIs=%s&" org-tfl-jp-arg-timeIs)
	  (format "journeyPreference=%s&" org-tfl-jp-arg-journeyPreference)
	  (if org-tfl-jp-arg-mode (format "mode=%s&" org-tfl-jp-arg-mode) "")
	  (if org-tfl-jp-arg-accessibilityPreference (format "accessibilityPreference=%s&"
					      org-tfl-jp-arg-accessibilityPreference) "")
	  (if org-tfl-jp-arg-fromName (format "fromName=%s&" org-tfl-jp-arg-fromName) "")
	  (if org-tfl-jp-arg-toName (format "toName=%s&" org-tfl-jp-arg-toName) "")
	  (if org-tfl-jp-arg-viaName (format "viaName=%s&" org-tfl-jp-arg-viaName) "")
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
	      (format "useMultiModalCall=%s&" org-tfl-jp-arg-useMultiModalCall) "")
))

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

(cl-defun org-tfl-jp-retrieve
    (from to &key
	  (via nil) (nationalSearch nil) (date nil) (time nil) (timeIs "Departing")
	  (journeyPreference "leasttime") (mode nil) (accessibilityPreference nil)
	  (fromName nil) (toName nil) (viaName nil) (maxTransferMinutes nil)
	  (maxWalkingMinutes nil) (walkingSpeed "average") (cyclePreference nil)
	  (adjustment nil) (bikeProficiency nil) (alternativeCycle nil)
	  (alternativeWalking "True") (applyHtmlMarkup nil) (useMultiModalCall nil))
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
MODE comma seperated list, possible options \"public-bus,overground,train,tube,coach,dlr,cablecar,tram,river,walking,cycle\".
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
USEMULTIMODALCALL A boolean to indicate whether or not to return 3 public transport journeys, a bus journey, a cycle hire journey, a personal cycle journey and a walking journey."
  (setq org-tfl-jp-arg-from from)
  (setq org-tfl-jp-arg-to to)
  (setq org-tfl-jp-arg-via via)
  (setq org-tfl-jp-arg-nationalSearch nationalSearch)
  (setq org-tfl-jp-arg-date date)
  (setq org-tfl-jp-arg-time time)
  (setq org-tfl-jp-arg-timeIs timeIs)
  (setq org-tfl-jp-arg-journeyPreference journeyPreference)
  (setq org-tfl-jp-arg-mode mode)
  (setq org-tfl-jp-arg-accessibilityPreference accessibilityPreference)
  (setq org-tfl-jp-arg-fromName fromName)
  (setq org-tfl-jp-arg-toName toName)
  (setq org-tfl-jp-arg-viaName viaName)
  (setq org-tfl-jp-arg-maxTransferMinutes maxTransferMinutes)
  (setq org-tfl-jp-arg-maxWalkingMinutes maxWalkingMinutes)
  (setq org-tfl-jp-arg-walkingSpeed walkingSpeed)
  (setq org-tfl-jp-arg-cyclePreference cyclePreference)
  (setq org-tfl-jp-arg-adjustment adjustment)
  (setq org-tfl-jp-arg-bikeProficiency bikeProficiency)
  (setq org-tfl-jp-arg-alternativeCycle alternativeCycle)
  (setq org-tfl-jp-arg-alternativeWalking alternativeWalking)
  (setq org-tfl-jp-arg-applyHtmlMarkup applyHtmlMarkup)
  (setq org-tfl-jp-arg-useMultiModalCall useMultiModalCall)

  (setq org-tfl-jp-fromdis nil)
  (setq org-tfl-jp-todis nil)
  (setq org-tfl-jp-viadis nil)

  (url-retrieve
   (org-tfl-jp-make-url)
   'org-tfl-jp-handle))


;; Example calls
;; (org-tfl-jp-retrieve "lonlat:\"-0.13500003041,51.50990587838\"" "lonlat:\"-0.29547881328,51.57205666482\"")

;; (org-tfl-jp-retrieve "Piccadilly Circus" "Preston Road")


(provide 'org-tfl)
;;; org-tfl ends here
