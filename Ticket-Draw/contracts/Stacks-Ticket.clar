;; EventMaster - Decentralized Event Ticketing Platform
;; A comprehensive blockchain-based ticketing system that enables event organizers 
;; to create, manage, and sell tickets while providing secure ticket ownership,
;; transfers, and validation for attendees on the Stacks blockchain.

;; =============================================================================
;; CONSTANTS AND ERROR DEFINITIONS
;; =============================================================================

;; Contract ownership
(define-constant contract-administrator tx-sender)

;; Error constants with descriptive names
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-RESOURCE-NOT-FOUND (err u101))
(define-constant ERR-PERMISSION-DENIED (err u102))
(define-constant ERR-RESOURCE-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-EVENT-INACTIVE (err u105))
(define-constant ERR-TICKETS-SOLD-OUT (err u106))
(define-constant ERR-TICKET-ALREADY-USED (err u107))
(define-constant ERR-INVALID-INPUT-DATA (err u108))
(define-constant ERR-PAYMENT-TRANSFER-FAILED (err u109))
(define-constant ERR-MAXIMUM-CAPACITY-EXCEEDED (err u110))
(define-constant ERR-EVENT-DATE-PASSED (err u111))
(define-constant ERR-ADMIN-ONLY-OPERATION (err u112))

;; Business logic constants
(define-constant maximum-tickets-per-user u10)
(define-constant maximum-events-per-organizer u100)
(define-constant maximum-user-ticket-lists u50)
(define-constant default-platform-fee-basis-points u250) ;; 2.5%
(define-constant maximum-platform-fee-basis-points u1000) ;; 10%
(define-constant basis-points-divisor u10000)

;; =============================================================================
;; STATE VARIABLES
;; =============================================================================

(define-data-var current-event-identifier uint u1)
(define-data-var current-ticket-identifier uint u1)
(define-data-var platform-fee-basis-points uint default-platform-fee-basis-points)

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; Event information storage
(define-map event-registry
  { event-identifier: uint }
  {
    event-title: (string-ascii 100),
    event-description: (string-ascii 500),
    event-organizer: principal,
    ticket-price-micro-stx: uint,
    maximum-ticket-supply: uint,
    current-tickets-sold: uint,
    event-timestamp: uint,
    event-status-active: bool,
    event-venue-location: (string-ascii 200),
    creation-block-height: uint
  }
)

;; Ticket ownership and metadata
(define-map ticket-registry
  { ticket-identifier: uint }
  {
    associated-event-id: uint,
    ticket-owner-principal: principal,
    purchase-block-height: uint,
    ticket-used-status: bool,
    paid-price-micro-stx: uint,
    original-purchaser: principal
  }
)

;; User ticket collections per event
(define-map user-event-tickets
  { user-principal: principal, event-identifier: uint }
  { owned-ticket-identifiers: (list 50 uint) }
)

;; Organizer event collections
(define-map organizer-event-portfolio
  { organizer-principal: principal }
  { created-event-identifiers: (list 100 uint) }
)

;; Event ticket availability tracking
(define-map event-availability-status
  { event-identifier: uint }
  { 
    is-sale-active: bool,
    remaining-ticket-count: uint,
    last-updated-block: uint
  }
)

;; =============================================================================
;; READ-ONLY QUERY FUNCTIONS
;; =============================================================================

(define-read-only (get-event-information (event-identifier uint))
  (map-get? event-registry { event-identifier: event-identifier })
)

(define-read-only (get-ticket-details (ticket-identifier uint))
  (map-get? ticket-registry { ticket-identifier: ticket-identifier })
)

(define-read-only (get-user-event-ticket-collection (user-principal principal) (event-identifier uint))
  (default-to 
    { owned-ticket-identifiers: (list) }
    (map-get? user-event-tickets { user-principal: user-principal, event-identifier: event-identifier })
  )
)

(define-read-only (get-organizer-event-collection (organizer-principal principal))
  (default-to 
    { created-event-identifiers: (list) }
    (map-get? organizer-event-portfolio { organizer-principal: organizer-principal })
  )
)

(define-read-only (get-current-platform-fee-percentage)
  (var-get platform-fee-basis-points)
)

(define-read-only (calculate-platform-fee-amount (ticket-price-micro-stx uint))
  (/ (* ticket-price-micro-stx (var-get platform-fee-basis-points)) basis-points-divisor)
)

(define-read-only (get-next-available-event-id)
  (var-get current-event-identifier)
)

(define-read-only (get-next-available-ticket-id)
  (var-get current-ticket-identifier)
)

(define-read-only (check-event-ticket-availability (event-identifier uint))
  (match (get-event-information event-identifier)
    event-data 
    (and 
      (get event-status-active event-data)
      (< (get current-tickets-sold event-data) (get maximum-ticket-supply event-data))
      (> (get event-timestamp event-data) stacks-block-height)
    )
    false
  )
)

(define-read-only (get-event-remaining-tickets (event-identifier uint))
  (match (get-event-information event-identifier)
    event-data 
    (some (- (get maximum-ticket-supply event-data) (get current-tickets-sold event-data)))
    none
  )
)

(define-read-only (verify-ticket-ownership (ticket-identifier uint) (user-principal principal))
  (match (get-ticket-details ticket-identifier)
    ticket-data
    (is-eq (get ticket-owner-principal ticket-data) user-principal)
    false
  )
)

(define-read-only (get-contract-administrator)
  contract-administrator
)

;; =============================================================================
;; PRIVATE UTILITY FUNCTIONS
;; =============================================================================

(define-private (add-ticket-to-user-collection (user-principal principal) (event-identifier uint) (ticket-identifier uint))
  (let ((current-user-tickets (get owned-ticket-identifiers (get-user-event-ticket-collection user-principal event-identifier))))
    (match (as-max-len? (append current-user-tickets ticket-identifier) u50)
      updated-tickets
      (begin
        (map-set user-event-tickets
          { user-principal: user-principal, event-identifier: event-identifier }
          { owned-ticket-identifiers: updated-tickets }
        )
        (ok true)
      )
      (err ERR-MAXIMUM-CAPACITY-EXCEEDED)
    )
  )
)

(define-private (add-event-to-organizer-portfolio (organizer-principal principal) (event-identifier uint))
  (let ((current-organizer-events (get created-event-identifiers (get-organizer-event-collection organizer-principal))))
    (match (as-max-len? (append current-organizer-events event-identifier) u100)
      updated-events
      (begin
        (map-set organizer-event-portfolio
          { organizer-principal: organizer-principal }
          { created-event-identifiers: updated-events }
        )
        (ok true)
      )
      (err ERR-MAXIMUM-CAPACITY-EXCEEDED)
    )
  )
)

(define-private (increment-event-ticket-sales (event-identifier uint))
  (match (get-event-information event-identifier)
    event-data
    (map-set event-registry
      { event-identifier: event-identifier }
      (merge event-data { current-tickets-sold: (+ (get current-tickets-sold event-data) u1) })
    )
    false
  )
)

(define-private (update-event-availability-cache (event-identifier uint))
  (match (get-event-information event-identifier)
    event-data
    (map-set event-availability-status
      { event-identifier: event-identifier }
      {
        is-sale-active: (get event-status-active event-data),
        remaining-ticket-count: (- (get maximum-ticket-supply event-data) (get current-tickets-sold event-data)),
        last-updated-block: stacks-block-height
      }
    )
    false
  )
)

(define-private (validate-event-creation-parameters 
  (event-title (string-ascii 100))
  (maximum-ticket-supply uint)
  (event-timestamp uint)
)
  (and 
    (> (len event-title) u0)
    (> maximum-ticket-supply u0)
    (> event-timestamp stacks-block-height)
    (<= maximum-ticket-supply u10000) ;; Reasonable upper limit
  )
)

(define-private (validate-string-input (input-string (string-ascii 500)))
  (> (len input-string) u0)
)

(define-private (validate-venue-input (venue-string (string-ascii 200)))
  (> (len venue-string) u0)
)

(define-private (validate-price-input (price uint))
  (>= price u0)
)

(define-private (validate-event-id-input (event-id uint))
  (> event-id u0)
)

(define-private (process-ticket-payment (ticket-price-micro-stx uint) (event-organizer-principal principal))
  (let (
    (platform-fee-amount (calculate-platform-fee-amount ticket-price-micro-stx))
    (organizer-payment-amount (- ticket-price-micro-stx platform-fee-amount))
  )
    (and
      (if (> organizer-payment-amount u0)
        (is-ok (stx-transfer? organizer-payment-amount tx-sender event-organizer-principal))
        true
      )
      (if (> platform-fee-amount u0)
        (is-ok (stx-transfer? platform-fee-amount tx-sender contract-administrator))
        true
      )
    )
  )
)

;; =============================================================================
;; EVENT MANAGEMENT FUNCTIONS
;; =============================================================================

(define-public (create-new-event 
  (event-title (string-ascii 100))
  (event-description (string-ascii 500))
  (ticket-price-micro-stx uint)
  (maximum-ticket-supply uint)
  (event-timestamp uint)
  (event-venue-location (string-ascii 200))
)
  (let ((new-event-identifier (var-get current-event-identifier)))
    (asserts! (validate-event-creation-parameters event-title maximum-ticket-supply event-timestamp) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-string-input event-description) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-price-input ticket-price-micro-stx) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-venue-input event-venue-location) ERR-INVALID-INPUT-DATA)
    
    (map-set event-registry
      { event-identifier: new-event-identifier }
      {
        event-title: event-title,
        event-description: event-description,
        event-organizer: tx-sender,
        ticket-price-micro-stx: ticket-price-micro-stx,
        maximum-ticket-supply: maximum-ticket-supply,
        current-tickets-sold: u0,
        event-timestamp: event-timestamp,
        event-status-active: true,
        event-venue-location: event-venue-location,
        creation-block-height: stacks-block-height
      }
    )
    
    (unwrap! (add-event-to-organizer-portfolio tx-sender new-event-identifier) ERR-MAXIMUM-CAPACITY-EXCEEDED)
    (update-event-availability-cache new-event-identifier)
    (var-set current-event-identifier (+ new-event-identifier u1))
    (ok new-event-identifier)
  )
)

(define-public (update-event-information 
  (event-identifier uint)
  (event-title (string-ascii 100))
  (event-description (string-ascii 500))
  (event-venue-location (string-ascii 200))
)
  (let ((event-data (unwrap! (get-event-information event-identifier) ERR-RESOURCE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get event-organizer event-data)) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (> (len event-title) u0) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-string-input event-description) ERR-INVALID-INPUT-DATA)
    (asserts! (validate-venue-input event-venue-location) ERR-INVALID-INPUT-DATA)
    
    (map-set event-registry
      { event-identifier: event-identifier }
      (merge event-data { 
        event-title: event-title,
        event-description: event-description,
        event-venue-location: event-venue-location
      })
    )
    
    (ok true)
  )
)

(define-public (toggle-event-status (event-identifier uint))
  (let ((event-data (unwrap! (get-event-information event-identifier) ERR-RESOURCE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get event-organizer event-data)) ERR-UNAUTHORIZED-ACCESS)
    
    (map-set event-registry
      { event-identifier: event-identifier }
      (merge event-data { event-status-active: (not (get event-status-active event-data)) })
    )
    
    (update-event-availability-cache event-identifier)
    (ok (not (get event-status-active event-data)))
  )
)

;; =============================================================================
;; TICKET PURCHASING AND MANAGEMENT FUNCTIONS
;; =============================================================================

(define-public (purchase-event-ticket (event-identifier uint))
  (let (
    (event-data (unwrap! (get-event-information event-identifier) ERR-RESOURCE-NOT-FOUND))
    (new-ticket-identifier (var-get current-ticket-identifier))
    (ticket-price (get ticket-price-micro-stx event-data))
  )
    (asserts! (get event-status-active event-data) ERR-EVENT-INACTIVE)
    (asserts! (< (get current-tickets-sold event-data) (get maximum-ticket-supply event-data)) ERR-TICKETS-SOLD-OUT)
    (asserts! (> (get event-timestamp event-data) stacks-block-height) ERR-EVENT-DATE-PASSED)
    
    ;; Process payment
    (asserts! (process-ticket-payment ticket-price (get event-organizer event-data)) ERR-PAYMENT-TRANSFER-FAILED)
    
    ;; Create ticket record
    (map-set ticket-registry
      { ticket-identifier: new-ticket-identifier }
      {
        associated-event-id: event-identifier,
        ticket-owner-principal: tx-sender,
        purchase-block-height: stacks-block-height,
        ticket-used-status: false,
        paid-price-micro-stx: ticket-price,
        original-purchaser: tx-sender
      }
    )
    
    ;; Update records
    (increment-event-ticket-sales event-identifier)
    (unwrap! (add-ticket-to-user-collection tx-sender event-identifier new-ticket-identifier) ERR-MAXIMUM-CAPACITY-EXCEEDED)
    (update-event-availability-cache event-identifier)
    (var-set current-ticket-identifier (+ new-ticket-identifier u1))
    
    (ok new-ticket-identifier)
  )
)

(define-public (transfer-ticket-ownership (ticket-identifier uint) (new-owner-principal principal))
  (let ((ticket-data (unwrap! (get-ticket-details ticket-identifier) ERR-RESOURCE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get ticket-owner-principal ticket-data)) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (get ticket-used-status ticket-data)) ERR-TICKET-ALREADY-USED)
    (asserts! (not (is-eq tx-sender new-owner-principal)) ERR-INVALID-INPUT-DATA)
    
    ;; Update ticket ownership
    (map-set ticket-registry
      { ticket-identifier: ticket-identifier }
      (merge ticket-data { ticket-owner-principal: new-owner-principal })
    )
    
    ;; Add ticket to new owner's collection
    (unwrap! (add-ticket-to-user-collection new-owner-principal (get associated-event-id ticket-data) ticket-identifier) ERR-MAXIMUM-CAPACITY-EXCEEDED)
    
    (ok true)
  )
)

(define-public (redeem-ticket (ticket-identifier uint))
  (let ((ticket-data (unwrap! (get-ticket-details ticket-identifier) ERR-RESOURCE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get ticket-owner-principal ticket-data)) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (get ticket-used-status ticket-data)) ERR-TICKET-ALREADY-USED)
    
    ;; Mark ticket as used
    (map-set ticket-registry
      { ticket-identifier: ticket-identifier }
      (merge ticket-data { ticket-used-status: true })
    )
    
    (ok true)
  )
)

;; =============================================================================
;; BATCH OPERATIONS
;; =============================================================================

(define-public (purchase-multiple-tickets (event-identifier uint) (ticket-quantity uint))
  (begin
    (asserts! (and (> ticket-quantity u0) (<= ticket-quantity maximum-tickets-per-user)) ERR-INVALID-INPUT-DATA)
    (let ((event-data (unwrap! (get-event-information event-identifier) ERR-RESOURCE-NOT-FOUND)))
      (asserts! (get event-status-active event-data) ERR-EVENT-INACTIVE)
      (asserts! (<= (+ (get current-tickets-sold event-data) ticket-quantity) (get maximum-ticket-supply event-data)) ERR-TICKETS-SOLD-OUT)
      
      (let ((result (fold execute-single-ticket-purchase 
                      (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) 
                      { 
                        event-identifier: event-identifier, 
                        remaining-purchases: ticket-quantity, 
                        purchased-tickets: (list),
                        success: true
                      })))
        (if (get success result)
          (ok (get purchased-tickets result))
          ERR-PAYMENT-TRANSFER-FAILED
        )
      )
    )
  )
)

(define-private (execute-single-ticket-purchase 
  (iteration uint) 
  (purchase-state { event-identifier: uint, remaining-purchases: uint, purchased-tickets: (list 10 uint), success: bool })
)
  (if (or (is-eq (get remaining-purchases purchase-state) u0) (not (get success purchase-state)))
    purchase-state
    (match (purchase-event-ticket (get event-identifier purchase-state))
      ticket-identifier 
      { 
        event-identifier: (get event-identifier purchase-state),
        remaining-purchases: (- (get remaining-purchases purchase-state) u1),
        purchased-tickets: (unwrap-panic (as-max-len? (append (get purchased-tickets purchase-state) ticket-identifier) u10)),
        success: true
      }
      error-val 
      (merge purchase-state { success: false })
    )
  )
)

;; =============================================================================
;; ADMINISTRATIVE FUNCTIONS
;; =============================================================================

(define-public (update-platform-fee-percentage (new-fee-basis-points uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-ADMIN-ONLY-OPERATION)
    (asserts! (<= new-fee-basis-points maximum-platform-fee-basis-points) ERR-INVALID-INPUT-DATA)
    (var-set platform-fee-basis-points new-fee-basis-points)
    (ok true)
  )
)

(define-public (emergency-disable-event (event-identifier uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-ADMIN-ONLY-OPERATION)
    (asserts! (validate-event-id-input event-identifier) ERR-INVALID-INPUT-DATA)
    (let ((event-data (unwrap! (get-event-information event-identifier) ERR-RESOURCE-NOT-FOUND)))
      (map-set event-registry
        { event-identifier: event-identifier }
        (merge event-data { event-status-active: false })
      )
      (update-event-availability-cache event-identifier)
      (ok true)
    )
  )
)

;; =============================================================================
;; ORGANIZER VERIFICATION FUNCTIONS
;; =============================================================================

(define-public (verify-event-organizer (event-identifier uint) (organizer-principal principal))
  (match (get-event-information event-identifier)
    event-data
    (ok (is-eq (get event-organizer event-data) organizer-principal))
    ERR-RESOURCE-NOT-FOUND
  )
)