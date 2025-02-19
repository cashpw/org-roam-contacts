;;; org-roam-contacts.el --- Manage contacts in org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: April 09, 2024
;; Modified: April 09, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashpw/org-roam-contacts
;; Package-Requires: ((emacs "24.3") (org-roam "2.2.2") (org-extras "0.0.1")
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Manage contacts in org-roam. We assume, until a future version, that you're using the 'one node per file' pattern.
;;
;;; Code:

(require 'org-roam)
(require 'org-extras)

(defgroup org-roam-contacts nil
  "Group for contacts."
  :tag "Contacts"
  :group 'org)

(defcustom org-roam-contacts--tag "person"
  "`org-roam' tag which indicates a contact node."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--birthday-property "CONTACT_BIRTHDAY"
  "Property name for a contact's birthday, stored as an org time string."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--email-property "CONTACT_EMAILS"
  "Property name for a contact's email addresses.

Each entry is a sexp of the form (LABEL . EMAIL_ADDRESS).

Example: ((\"Home\" . \"foo@bar.com\") (\"Work\" . \"foo.bar@company.com\"))."
  :type '(repeat sexp)
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--address-property "CONTACT_ADDRESSES"
  "Property name for a contact's addresses.

Each entry is a sexp of the form (LABEL . ADDRESS).

Example: ((\"Home\" . \"15 North Lane, ...\") (\"Work\" . \"48 Esplanade, ...\"))."
  :type 'string
  :group 'org-roam-contacts)

(defvaralias 'org-roam-contacts--tel-property
  'org-roam-contacts--phone-home-property)
(defcustom org-roam-contacts--phone-property "CONTACT_PHONES"
  "Property name for a contact's phone numbers.

Each entry is a sexp of the form (LABEL . PHONE_NUMBER).

Example: ((\"Home\" . \"+12223334444\") (\"Work\" . \"+15556667777\"))."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--reminders-heading "Reminders"
  "Heading text for reminders."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--birthday-reminder-heading "%s's birthday"
  "Heading text for birthday reminders.

The %s will be replaced with the contact's name."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--birthday-advanced-reminder-heading
  "%s's birthday in %d days"
  "Heading text for advanced birthday reminders.

The %s will be replaced with the contact's name."
  :type 'string
  :group 'org-roam-contacts)

(defcustom org-roam-contacts--directory org-roam-directory
  "Directory path."
  :type 'string
  :group 'org-roam-contacts)

(defun org-roam-contacts-properties ()
  "Return a list of properties used by `org-roam-contacts'."
  `(,org-roam-contacts--phone-property
    ,org-roam-contacts--address-property
    ,org-roam-contacts--email-property
    ,org-roam-contacts--birthday-property))

(defun org-roam-contacts--get-next-annual-time (time)
  "Return time string for the next annual recurrence of TIME."
  (if (not (time-less-p time (current-time)))
      time
    (cl-destructuring-bind
        (seconds
         minutes
         hours
         days
         months
         years
         day-of-week
         daylight-savings-time-p
         utc-offset)
        (decode-time time)
      (let* ((current-year (nth 5 (decode-time (current-time))))
             (next-year (1+ current-year)))
        (encode-time seconds
                     minutes
                     hours
                     days
                     months
                     next-year
                     day-of-week
                     daylight-savings-time-p
                     utc-offset)))))

(cl-letf (((symbol-function 'current-time)
           (lambda () (date-to-time "2022-10-05T08:00:00-0700"))))
  (cl-assert
   (equal
    (org-roam-contacts--get-next-annual-time
     (date-to-time "2022-10-10T08:00:00-0700"))
    (date-to-time "2022-10-10T08:00:00-0700"))
   "Next time should be this year (2022) because the date hasn't yet passed.")
  (cl-assert
   (equal
    (org-roam-contacts--get-next-annual-time
     (date-to-time "2022-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed.")
  (cl-assert
   (equal
    (org-roam-contacts--get-next-annual-time
     (date-to-time "2000-10-10T08:00:00-0700"))
    (date-to-time "2023-10-10T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed.")
  (cl-assert
   (equal
    (org-roam-contacts--get-next-annual-time
     (date-to-time "2000-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed."))

(defun org-roam-contacts--has-property-p (property)
  "Returns nil if the contact lacks the PROPERTY."
  (member property (org-buffer-property-keys)))

(defun org-roam-contacts--get-property (property)
  "Returns value of PROPERTY or nil if PROPERTY not found."
  (org-entry-get (point-min) property))

(defun org-roam-contacts--list-top-level-headings ()
  "Return list of top-level headings in buffer."
  (org-map-entries (lambda () (org-entry-get nil "ITEM")) "LEVEL=1"))

(defun org-roam-contacts--top-level-heading-exists-p (heading-text)
  "Return t if HEADING-TEXT is among top-level headings and nil otherwise."
  (member heading-text (org-roam-contacts--list-top-level-headings)))

(defun org-roam-contacts--insert-top-level-heading-if-absent
    (heading-text &optional pos)
  "Creates a top-level heading with HEADING-TEXT at POS if such a heading doesn't exist in buffer.

Returns nil if the heading already existed."
  (let ((pos (or pos (point-max))))
    (unless (org-roam-contacts--top-level-heading-exists-p heading-text)
      (goto-char pos)
      (org-insert-heading nil t t)
      (insert heading-text))))

(defun org-roam-contacts--goto-heading (heading-text)
  "Move pointer to the heading with HEADING-TEXT.

Does nothing if such a heading is absent."
  (let ((heading-position (org-find-exact-headline-in-buffer heading-text)))
    (when heading-position
      (goto-char heading-position))))

(defun org-roam-contacts-insert-reminder
    (reminder-text &optional time repeater-interval)
  "Insert a reminder with REMINDER-TEXT heading at TIME with REPEATER-INTERVAL."
  (interactive "sReminder heading: ")
  (let ((time-string
         (format-time-string (if repeater-interval
                                 (format "<%%F %s>" repeater-interval)
                               "<%F>")
                             (or time (org-read-date nil t)))))
    (save-excursion
      (org-roam-contacts--insert-top-level-heading-if-absent
       org-roam-contacts--reminders-heading)
      (org-roam-contacts--goto-heading org-roam-contacts--reminders-heading)
      (org-insert-todo-subheading nil)
      (insert reminder-text)
      (org-set-property
       "CREATED_AT"
       (format-time-string "[%Y-%m-%d %a %H:%M:%S]" (current-time)))
      (org-entry-put (point) "SCHEDULED" time-string))))

(defun org-roam-contacts-file-p ()
  "Contacts files are roam files in a specific directory."
  (and (org-roam-file-p)
       (member
        org-roam-contacts--tag (org-extras-filetags-in-buffer (buffer-name)))))

(defun org-roam-contacts--get-birthday-time ()
  "Get emacs time representation of the contact's birthday."
  (org-time-string-to-time
   (org-roam-contacts--get-property org-roam-contacts--birthday-property)))

(defun org-roam-contacts-insert-birthday-reminders (advance-notice-days)
  "Create the following birthday reminders:

1. Annually on the person's birthday
2. Annually ADVANCE-NOTICE-DAYS before the person's birthday"
  (interactive "nAdvance notice days: ")
  (when (and (org-roam-contacts-file-p)
             (org-roam-contacts--has-property-p
              org-roam-contacts--birthday-property))
    (let* ((birth-time (org-roam-contacts--get-birthday-time))
           (contact-name (org-roam-contacts--get-name))
           (birthday-heading-text
            (format org-roam-contacts--birthday-reminder-heading contact-name))
           (upcoming-birthday-heading-text
            (format org-roam-contacts--birthday-advanced-reminder-heading
                    contact-name advance-notice-days)))
      (unless (org-find-exact-headline-in-buffer upcoming-birthday-heading-text)
        (let* ((reminder-time
                (org-roam-contacts--get-next-annual-time
                 (time-subtract
                  birth-time (days-to-time advance-notice-days)))))
          (org-roam-contacts-insert-reminder upcoming-birthday-heading-text
                                             reminder-time
                                             "++1y")))
      (unless (org-find-exact-headline-in-buffer birthday-heading-text)
        (let* ((reminder-time
                (org-roam-contacts--get-next-annual-time birth-time)))
          (org-roam-contacts-insert-reminder birthday-heading-text
                                             reminder-time
                                             "++1y"))))))

(defun org-roam-contacts--get-name (&optional path)
  "Return name of contact at PATH.

Assumes the contact name is the title of the file."
  (let ((path (or path (buffer-file-name (buffer-base-buffer)))))
    (when path
      (with-current-buffer (get-file-buffer path)
        (pcase (org-collect-keywords '("TITLE"))
          (`(("TITLE" . ,val)) (car val)))))))

(provide 'org-roam-contacts)
;;; org-roam-contacts.el ends here
