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

;; Process single recipient transfer in bulk operation
(define-private (process-single-transfer (recipient principal) (hours uint))
  (let (
    (sender-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (recipient-balance (default-to u0 (map-get? creator-timeslot-holdings recipient)))
    (updated-recipient-balance (+ recipient-balance hours))
  )
    ;; Ensure recipient is not the sender
    (if (is-eq tx-sender recipient)
        (err err-identical-creator)
        ;; Ensure valid duration
        (if (<= hours u0)
            (err err-invalid-time-amount)
            ;; Check recipient won't exceed limit
            (if (> updated-recipient-balance (var-get user-timeslot-ceiling))
                (err err-timeslot-limit-reached)
                ;; Complete the transfer
                (begin
                  (map-set creator-timeslot-holdings recipient updated-recipient-balance)
                  (ok true)))))))

;; ========== TIMESLOT ACQUISITION FUNCTIONS ==========

;; Purchase timeslots with tokens
(define-public (acquire-creative-timeslots (hours uint))
  (let (
    (total-cost (* hours (var-get timeslot-base-cost)))
    (existing-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (updated-balance (+ existing-balance hours))
    (admin-balance (default-to u0 (map-get? creator-token-holdings marketplace-admin)))
  )
    ;; Validate request parameters
    (asserts! (> hours u0) err-invalid-time-amount)
    (asserts! (<= updated-balance (var-get user-timeslot-ceiling)) err-timeslot-limit-reached)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Process payment and update balances
    (try! (stx-transfer? total-cost tx-sender marketplace-admin))
    (try! (adjust-timeslot-allocation (to-int hours)))
    (map-set creator-timeslot-holdings tx-sender updated-balance)
    (map-set creator-token-holdings marketplace-admin (+ admin-balance total-cost))

    (ok true)))

;; Request refund for timeslots
(define-public (request-timeslot-refund (hours uint))
  (let (
    (creator-timeslots (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (refund-amount (compute-cancellation-refund hours))
    (admin-token-balance (default-to u0 (map-get? creator-token-holdings marketplace-admin)))
  )
    ;; Validate refund request
    (asserts! (> hours u0) err-invalid-time-amount)
    (asserts! (>= creator-timeslots hours) err-insufficient-timeslots)
    (asserts! (>= admin-token-balance refund-amount) err-cancellation-failed)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Process refund
    (map-set creator-timeslot-holdings tx-sender (- creator-timeslots hours))
    (map-set creator-token-holdings tx-sender (+ refund-amount))

    (ok true)))

;; ========== MARKETPLACE FUNCTIONS ==========

;; List available timeslots on marketplace
(define-public (list-timeslots-for-rental (hours uint) (rate uint))
  (let (
    (current-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (current-listing (get duration (default-to {duration: u0, rate: u0} (map-get? timeslot-marketplace {creator: tx-sender}))))
    (total-listing (+ hours current-listing))
  )
    ;; Validate listing parameters
    (asserts! (> hours u0) err-invalid-time-amount)
    (asserts! (> rate u0) err-invalid-rate)
    (asserts! (>= current-balance total-listing) err-insufficient-timeslots)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update capacity tracking
    (try! (adjust-timeslot-allocation (to-int hours)))

    ;; Update marketplace listing
    (map-set timeslot-marketplace {creator: tx-sender} {duration: total-listing, rate: rate})

    (ok true)))

;; Remove timeslot listing from marketplace
(define-public (delist-timeslots)
  (let (
    (listing-data (default-to {duration: u0, rate: u0} (map-get? timeslot-marketplace {creator: tx-sender})))
    (listed-hours (get duration listing-data))
    (creator-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
  )
    ;; Validate delisting request
    (asserts! (> listed-hours u0) err-insufficient-timeslots)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Remove listing
    (map-delete timeslot-marketplace {creator: tx-sender})

    ;; Return timeslots to creator's balance
    (map-set creator-timeslot-holdings tx-sender (+ creator-balance listed-hours))

    (ok true)))

;; Rent timeslots from another creator
(define-public (rent-creator-timeslots (provider principal) (hours uint))
  (let (
    (listing-data (default-to {duration: u0, rate: u0} (map-get? timeslot-marketplace {creator: provider})))
    (rental-cost (* hours (get rate listing-data)))
    (platform-fee (compute-platform-fee rental-cost))
    (total-cost (+ rental-cost platform-fee))
    (provider-timeslots (default-to u0 (map-get? creator-timeslot-holdings provider)))
    (renter-tokens (default-to u0 (map-get? creator-token-holdings tx-sender)))
    (provider-tokens (default-to u0 (map-get? creator-token-holdings provider)))
    (admin-tokens (default-to u0 (map-get? creator-token-holdings marketplace-admin)))
  )
    ;; Validate rental request
    (asserts! (not (is-eq tx-sender provider)) err-identical-creator)
    (asserts! (> hours u0) err-invalid-time-amount)
    (asserts! (>= (get duration listing-data) hours) err-insufficient-timeslots)
    (asserts! (>= provider-timeslots hours) err-insufficient-timeslots)
    (asserts! (>= renter-tokens total-cost) err-insufficient-timeslots)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update provider's timeslot balance and listing
    (map-set creator-timeslot-holdings provider (- provider-timeslots hours))
    (map-set timeslot-marketplace {creator: provider} 
             {duration: (- (get duration listing-data) hours), rate: (get rate listing-data)})

    ;; Update token balances
    (map-set creator-token-holdings tx-sender (- renter-tokens total-cost))
    (map-set creator-timeslot-holdings tx-sender (+ (default-to u0 (map-get? creator-timeslot-holdings tx-sender)) hours))
    (map-set creator-token-holdings provider (+ provider-tokens rental-cost))
    (map-set creator-token-holdings marketplace-admin (+ admin-tokens platform-fee))

    (ok true)))

;; ========== SESSION MANAGEMENT ==========

;; Start a timeslot session
(define-public (begin-timeslot-session (hours uint))
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (creator-timeslots (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (session-data (default-to {entry-timestamp: u0, duration: u0, session-active: false} 
                            (map-get? active-timeslot-sessions {creator: tx-sender})))
  )
    ;; Validate session request
    (asserts! (>= creator-timeslots hours) err-insufficient-timeslots)
    (asserts! (not (get session-active session-data)) err-already-in-session)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Deduct timeslots from creator's balance
    (map-set creator-timeslot-holdings tx-sender (- creator-timeslots hours))

    ;; Record session details
    (map-set active-timeslot-sessions {creator: tx-sender} 
             {entry-timestamp: current-time, duration: hours, session-active: true})

    (ok true)))

;; End a timeslot session
(define-public (end-timeslot-session (return-unused bool))
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (session-data (default-to {entry-timestamp: u0, duration: u0, session-active: false} 
                            (map-get? active-timeslot-sessions {creator: tx-sender})))
    (start-time (get entry-timestamp session-data))
    (reserved-hours (get duration session-data))
    (is-active (get session-active session-data))
    (elapsed-seconds (- current-time start-time))
    (elapsed-hours (/ elapsed-seconds u3600))
    (remaining-hours (if (< elapsed-hours reserved-hours)
                      (- reserved-hours elapsed-hours)
                      u0))
  )
    ;; Validate session end request
    (asserts! is-active err-not-in-session)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Mark session as ended
    (map-set active-timeslot-sessions {creator: tx-sender} 
             {entry-timestamp: u0, duration: u0, session-active: false})

    ;; Return unused time if requested
    (if (and return-unused (> remaining-hours u0))
        (let (
            (current-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
        )
          (map-set creator-timeslot-holdings tx-sender (+ current-balance remaining-hours))
          (ok remaining-hours))
        (ok u0))
  ))

;; ========== TIMESLOT TRANSFER FUNCTIONS ==========

;; Transfer timeslots to another creator
(define-public (transfer-creative-timeslots (recipient principal) (hours uint))
  (let (
    (sender-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (recipient-balance (default-to u0 (map-get? creator-timeslot-holdings recipient)))
    (updated-recipient-balance (+ recipient-balance hours))
  )
    ;; Validate transfer request
    (asserts! (not (is-eq tx-sender recipient)) err-identical-creator)
    (asserts! (> hours u0) err-invalid-time-amount)
    (asserts! (>= sender-balance hours) err-insufficient-timeslots)
    (asserts! (<= updated-recipient-balance (var-get user-timeslot-ceiling)) err-timeslot-limit-reached)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update balances
    (map-set creator-timeslot-holdings tx-sender (- sender-balance hours))
    (map-set creator-timeslot-holdings recipient updated-recipient-balance)

    (ok true)))

;; Batch transfer timeslots to multiple recipients
(define-public (batch-transfer-timeslots (recipients (list 20 principal)) (hours-per-recipient uint))
  (let (
    (sender-balance (default-to u0 (map-get? creator-timeslot-holdings tx-sender)))
    (recipient-count (len recipients))
    (total-hours (* recipient-count hours-per-recipient))
  )
    ;; Validate batch transfer request
    (asserts! (> recipient-count u0) err-empty-transfer-list)
    (asserts! (<= recipient-count max-batch-recipients) err-recipient-limit-exceeded)
    (asserts! (> hours-per-recipient u0) err-invalid-time-amount)
    (asserts! (>= sender-balance total-hours) err-insufficient-timeslots)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update sender's balance first
    (map-set creator-timeslot-holdings tx-sender (- sender-balance total-hours))

    ;; Process each recipient's transfer
    (map process-single-transfer recipients (list recipient-count hours-per-recipient))

    (ok true)))

;; ========== TOKEN MANAGEMENT ==========

;; Withdraw tokens from platform
(define-public (withdraw-tokens (amount uint))
  (let (
    (creator-balance (default-to u0 (map-get? creator-token-holdings tx-sender)))
  )
    ;; Validate withdrawal request
    (asserts! (> amount u0) err-invalid-rate)
    (asserts! (>= creator-balance amount) err-insufficient-timeslots)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update token balance and transfer tokens
    (map-set creator-token-holdings tx-sender (- creator-balance amount))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    (ok true)))

;; ========== PLATFORM ADMINISTRATION ==========

;; Pause platform operations
(define-public (suspend-marketplace)
  (begin
    ;; Verify admin credentials
    (asserts! (is-eq tx-sender marketplace-admin) err-admin-restricted)

    ;; Ensure platform is not already paused
    (asserts! (not (var-get marketplace-active)) err-already-paused)

    ;; Set platform state to paused
    (var-set marketplace-active true)

    (ok true)))

;; Resume platform operations
(define-public (resume-marketplace)
  (begin
    ;; Verify admin credentials
    (asserts! (is-eq tx-sender marketplace-admin) err-admin-restricted)

    ;; Ensure platform is currently paused
    (asserts! (var-get marketplace-active) err-already-active)

    ;; Set platform state to active
    (var-set marketplace-active false)

    (ok true)))

;; Update platform configuration
(define-public (update-marketplace-parameters (new-base-cost (optional uint)) 
                                             (new-fee-percentage (optional uint))
                                             (new-refund-percentage (optional uint))
                                             (new-user-ceiling (optional uint))
                                             (new-total-capacity (optional uint)))
  (begin
    ;; Verify admin credentials
    (asserts! (is-eq tx-sender marketplace-admin) err-admin-restricted)
    (asserts! (not (var-get marketplace-active)) err-marketplace-paused)

    ;; Update base cost if provided
    (if (is-some new-base-cost)
        (let ((cost (unwrap! new-base-cost err-invalid-rate)))
          (asserts! (> cost u0) err-invalid-rate)
          (var-set timeslot-base-cost cost))
        true)

    ;; Update fee percentage if provided
    (if (is-some new-fee-percentage)
        (let ((percentage (unwrap! new-fee-percentage err-invalid-percentage)))
          (asserts! (<= percentage u20) err-invalid-percentage)
          (var-set platform-fee-percentage percentage))
        true)

    ;; Update refund percentage if provided
    (if (is-some new-refund-percentage)
        (let ((percentage (unwrap! new-refund-percentage err-invalid-percentage)))
          (asserts! (<= percentage u100) err-invalid-percentage)
          (var-set cancellation-return-percentage percentage))
        true)

    ;; Update user ceiling if provided
    (if (is-some new-user-ceiling)
        (let ((ceiling (unwrap! new-user-ceiling err-invalid-capacity-limit)))
          (asserts! (> ceiling u0) err-invalid-capacity-limit)
          (var-set user-timeslot-ceiling ceiling))
        true)

    ;; Update total capacity if provided
    (if (is-some new-total-capacity)
        (let ((capacity (unwrap! new-total-capacity err-invalid-capacity-limit)))
          (asserts! (>= capacity (var-get allocated-timeslots)) err-invalid-capacity-limit)
          (var-set total-timeslot-capacity capacity))
        true)

    (ok true)))

;; ========== END OF CONTRACT ==========
;; ArtisanLoft: Creating spaces for creative professionals to thrive

