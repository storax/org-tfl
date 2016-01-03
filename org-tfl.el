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

(cl-defun org-tfl-jp-make-url
    (from to &key
	  (via nil) (nationalSearch nil) (date nil) (time nil) (timeIs "Departing")
	  (journeyPreference "leasttime") (mode nil) (accessibilityPreference nil)
	  (fromName nil) (toName nil) (viaName nil) (maxTransferMinutes nil)
	  (maxWalkingMinutes nil) (walkingSpeed "average") (cyclePreference nil)
	  (adjustment nil) (bikeProficiency nil) (alternativeCycle nil)
	  (alternativeWalking t) (applyHtmlMarkup nil) (useMultiModalCall nil))
  "Create journey planner url FROM TO.

For keys see 'org-tfl-jp-retrieve'."
  (concat org-tfl-api-base-url
	  (format org-tfl-api-jp from to)
	  "?"
	  (if (and org-tfl-api-jp org-tfl-api-key)
	      (format "app_id=%s&app_key=%s&" (or org-tfl-api-id "") (or org-tfl-api-key ""))
	    "")
	  (if via (format "via=%s&" via) "")
	  (if nationalSearch (format "nationalSearch=%s&" nationalSearch) "")
	  (if date (format "date=%s&" date) "")
	  (if time (format "time=%s&" time) "")
	  (format "timeIs=%s&" timeIs)
	  (format "journeyPreference=%s&" journeyPreference)
	  (if mode (format "mode=%s&" mode) "")
	  (if accessibilityPreference (format "accessibilityPreference=%s&" accessibilityPreference) "")
	  (if fromName (format "fromName=%s&" fromName) "")
	  (if toName (format "toName=%s&" toName) "")
	  (if viaName (format "viaName=%s&" viaName) "")
	  (if maxTransferMinutes (format "maxTransferMinutes=%s&" maxTransferMinutes) "")
	  (if maxWalkingMinutes (format "maxWalkingMinutes=%s&" maxWalkingMinutes) "")
	  (format "average=%s&" walkingSpeed)
	  (if cyclePreference (format "cyclePreference=%s&" cyclePreference) "")
	  (if adjustment (format "adjustment=%s&" adjustment) "")
	  (if bikeProficiency (format "bikeProficiency=%s&" bikeProficiency) "")
	  (if alternativeCycle (format "alternativeCycle=%s&" alternativeCycle) "")
	  (if alternativeWalking (format "alternativeWalking=%s&" alternativeWalking) "")
	  (if applyHtmlMarkup (format "applyHtmlMarkup=%s&" applyHtmlMarkup) "")
	  (if useMultiModalCall (format "useMultiModalCall=%s&" useMultiModalCall) "")
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
NATIONALSEARCH should be t for journeys outside London.
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
  (url-retrieve
   (org-tfl-jp-make-url from to
			:via via :nationalSearch nationalSearch
			:date date :time time :timeIs timeIs :journeyPreference journeyPreference
			:mode mode :accessibilityPreference accessibilityPreference
			:fromName fromName :toName toName :viaName viaName
			:maxTransferMinutes maxTransferMinutes
			:maxWalkingMinutes maxWalkingMinutes :walkingSpeed walkingSpeed
			:cyclePreference cyclePreference :adjustment adjustment
			:bikeProficiency bikeProficiency :alternativeCycle alternativeCycle
			:alternativeWalking alternativeWalking :applyHtmlMarkup applyHtmlMarkup
			:useMultiModalCall useMultiModalCall)
   'org-tfl-jp-handle))


;; Example calls
;; (org-tfl-jp-retrieve "lonlat:\"-0.13500003041,51.50990587838\"" "lonlat:\"-0.29547881328,51.57205666482\"")

;; (org-tfl-jp-retrieve "Piccadilly Circus" "Preston Road")


(provide 'org-tfl)
;;; org-tfl ends here
