;; Pulse Point - Micro-Duration Exchange Platform

;; ========== PLATFORM SETTINGS ==========
;; Base configuration for the platform operation
(define-data-var timeslot-base-cost uint u500)
(define-data-var user-timeslot-ceiling uint u100)
(define-data-var platform-fee-percentage uint u5)
(define-data-var cancellation-return-percentage uint u90)
(define-data-var total-timeslot-capacity uint u10000)
(define-data-var allocated-timeslots uint u0)

;; ========== STATE TRACKING ==========
;; Maps to track platform state and user interactions
(define-map creator-timeslot-holdings principal uint)
(define-map creator-token-holdings principal uint)
(define-map timeslot-marketplace {creator: principal} {duration: uint, rate: uint})
(define-map active-timeslot-sessions {creator: principal} {entry-timestamp: uint, duration: uint, session-active: bool})

;; ========== CONSTANTS ==========
;; Platform constants and error codes
(define-constant marketplace-admin tx-sender)
(define-constant err-admin-restricted (err u100))
(define-constant err-insufficient-timeslots (err u101))
(define-constant err-booking-failed (err u102))
(define-constant err-invalid-rate (err u103))
(define-constant err-invalid-time-amount (err u104))
(define-constant err-invalid-percentage (err u105))
(define-constant err-cancellation-failed (err u106))
(define-constant err-identical-creator (err u107))
(define-constant err-timeslot-limit-reached (err u108))
(define-constant err-invalid-capacity-limit (err u109))
(define-constant err-already-in-session (err u110))
(define-constant err-no-timeslots-available (err u111))
(define-constant err-not-in-session (err u112))
(define-constant err-early-exit-disabled (err u113))
(define-constant err-marketplace-paused (err u116))
(define-constant err-already-paused (err u117))
(define-constant err-already-active (err u118))
(define-constant err-invalid-transfer-list (err u119))
(define-constant err-transfer-failed (err u120))
(define-constant err-empty-transfer-list (err u121))
(define-constant err-recipient-limit-exceeded (err u122))

;; Additional platform state
(define-data-var marketplace-active bool true)
(define-constant max-batch-recipients u20)

;; ========== HELPER FUNCTIONS ==========
;; Private utility functions for internal calculations and operations

;; Calculate platform fee for a transaction
(define-private (compute-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u100))

;; Calculate cancellation refund amount
(define-private (compute-cancellation-refund (amount uint))
  (/ (* amount (var-get timeslot-base-cost) (var-get cancellation-return-percentage)) u100))

;; Track allocated timeslots in the system
(define-private (adjust-timeslot-allocation (hours int))
  (let (
    (current-allocation (var-get allocated-timeslots))
    (updated-allocation (if (< hours 0)
                         (if (>= current-allocation (to-uint (- 0 hours)))
                             (- current-allocation (to-uint (- 0 hours)))
                             u0)
                         (+ current-allocation (to-uint hours))))
  )
    (asserts! (<= updated-allocation (var-get total-timeslot-capacity)) err-timeslot-limit-reached)
    (var-set allocated-timeslots updated-allocation)
    (ok true)))
