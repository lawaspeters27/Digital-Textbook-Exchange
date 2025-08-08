;; ===============================================
;; DIGITAL TEXTBOOK EXCHANGE MARKETPLACE
;; ===============================================
;; A comprehensive marketplace for students to buy and sell used textbooks
;; Features: listings, messaging, condition ratings, semester categorization

;; ===============================================
;; CONSTANTS & ERROR CODES
;; ===============================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LISTING-NOT-FOUND (err u101))
(define-constant ERR-LISTING-NOT-ACTIVE (err u102))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u103))
(define-constant ERR-CANNOT-BUY-OWN-LISTING (err u104))
(define-constant ERR-MESSAGE-NOT-FOUND (err u105))
(define-constant ERR-INVALID-CONDITION (err u106))
(define-constant ERR-INVALID-SEMESTER (err u107))
(define-constant ERR-LISTING-ALREADY-SOLD (err u108))
(define-constant ERR-TRANSFER-FAILED (err u109))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Platform fee (2.5% in basis points)
(define-constant PLATFORM-FEE u250)
(define-constant BASIS-POINTS u10000)

;; Condition ratings (1-5 scale)
(define-constant CONDITION-POOR u1)
(define-constant CONDITION-FAIR u2)
(define-constant CONDITION-GOOD u3)
(define-constant CONDITION-VERY-GOOD u4)
(define-constant CONDITION-EXCELLENT u5)

;; Semester codes
(define-constant SEMESTER-SPRING u1)
(define-constant SEMESTER-SUMMER u2)
(define-constant SEMESTER-FALL u3)
(define-constant SEMESTER-WINTER u4)

;; ===============================================
;; DATA STRUCTURES
;; ===============================================

;; Textbook listing data
(define-map listings
    { listing-id: uint }
    {
        seller: principal,
        title: (string-ascii 256),
        author: (string-ascii 128),
        isbn: (string-ascii 20),
        edition: uint,
        subject: (string-ascii 64),
        course-code: (string-ascii 16),
        price-micro-stx: uint,
        condition: uint,
        semester: uint,
        year: uint,
        description: (string-ascii 512),
        is-active: bool,
        is-sold: bool,
        created-at: uint,
        sold-at: (optional uint),
        buyer: (optional principal)
    }
)

;; Message thread between buyer and seller
(define-map messages
    { message-id: uint }
    {
        listing-id: uint,
        sender: principal,
        recipient: principal,
        content: (string-ascii 512),
        timestamp: uint,
        is-read: bool
    }
)

;; User ratings and transaction history
(define-map user-profiles
    { user: principal }
    {
        total-listings: uint,
        total-purchases: uint,
        total-sales: uint,
        average-rating: uint,
        total-ratings: uint,
        reputation-score: uint,
        joined-at: uint
    }
)

;; Transaction records for analytics and dispute resolution
(define-map transactions
    { transaction-id: uint }
    {
        listing-id: uint,
        seller: principal,
        buyer: principal,
        price-paid: uint,
        platform-fee-paid: uint,
        completed-at: uint,
        rating-given: (optional uint)
    }
)

;; ===============================================
;; DATA VARIABLES
;; ===============================================

(define-data-var next-listing-id uint u1)
(define-data-var next-message-id uint u1)
(define-data-var next-transaction-id uint u1)
(define-data-var platform-fee-collected uint u0)
(define-data-var total-volume uint u0)

;; ===============================================
;; PRIVATE HELPER FUNCTIONS
;; ===============================================

(define-private (is-valid-condition (condition uint))
    (and (>= condition CONDITION-POOR) (<= condition CONDITION-EXCELLENT))
)

(define-private (is-valid-semester (semester uint))
    (and (>= semester SEMESTER-SPRING) (<= semester SEMESTER-WINTER))
)

(define-private (calculate-platform-fee (price uint))
    (/ (* price PLATFORM-FEE) BASIS-POINTS)
)

(define-private (update-user-profile-on-listing (user principal))
    (let ((current-profile (default-to
            { total-listings: u0, total-purchases: u0, total-sales: u0,
              average-rating: u0, total-ratings: u0, reputation-score: u0,
              joined-at: stacks-block-height }
            (map-get? user-profiles { user: user }))))
        (map-set user-profiles
            { user: user }
            (merge current-profile {
                total-listings: (+ (get total-listings current-profile) u1)
            })
        )
    )
)

(define-private (update-user-profile-on-purchase (buyer principal) (seller principal))
    (begin
        ;; Update buyer profile
        (let ((buyer-profile (default-to
                { total-listings: u0, total-purchases: u0, total-sales: u0,
                  average-rating: u0, total-ratings: u0, reputation-score: u0,
                  joined-at: stacks-block-height }
                (map-get? user-profiles { user: buyer }))))
            (map-set user-profiles
                { user: buyer }
                (merge buyer-profile {
                    total-purchases: (+ (get total-purchases buyer-profile) u1),
                    reputation-score: (+ (get reputation-score buyer-profile) u1)
                })
            )
        )
        ;; Update seller profile
        (let ((seller-profile (default-to
                { total-listings: u0, total-purchases: u0, total-sales: u0,
                  average-rating: u0, total-ratings: u0, reputation-score: u0,
                  joined-at: stacks-block-height }
                (map-get? user-profiles { user: seller }))))
            (map-set user-profiles
                { user: seller }
                (merge seller-profile {
                    total-sales: (+ (get total-sales seller-profile) u1),
                    reputation-score: (+ (get reputation-score seller-profile) u2)
                })
            )
        )
    )
)

;; ===============================================
;; PUBLIC FUNCTIONS - LISTING MANAGEMENT
;; ===============================================

(define-public (create-listing
    (title (string-ascii 256))
    (author (string-ascii 128))
    (isbn (string-ascii 20))
    (edition uint)
    (subject (string-ascii 64))
    (course-code (string-ascii 16))
    (price-micro-stx uint)
    (condition uint)
    (semester uint)
    (year uint)
    (description (string-ascii 512))
)
    (let ((listing-id (var-get next-listing-id)))
        (asserts! (is-valid-condition condition) ERR-INVALID-CONDITION)
        (asserts! (is-valid-semester semester) ERR-INVALID-SEMESTER)
        (asserts! (> price-micro-stx u0) ERR-INSUFFICIENT-PAYMENT)

        (map-set listings
            { listing-id: listing-id }
            {
                seller: tx-sender,
                title: title,
                author: author,
                isbn: isbn,
                edition: edition,
                subject: subject,
                course-code: course-code,
                price-micro-stx: price-micro-stx,
                condition: condition,
                semester: semester,
                year: year,
                description: description,
                is-active: true,
                is-sold: false,
                created-at: stacks-block-height,
                sold-at: none,
                buyer: none
            }
        )

        (update-user-profile-on-listing tx-sender)
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id)
    )
)

(define-public (update-listing-price (listing-id uint) (new-price-micro-stx uint))
    (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
        (asserts! (is-eq (get seller listing) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active listing) ERR-LISTING-NOT-ACTIVE)
        (asserts! (not (get is-sold listing)) ERR-LISTING-ALREADY-SOLD)
        (asserts! (> new-price-micro-stx u0) ERR-INSUFFICIENT-PAYMENT)

        (map-set listings
            { listing-id: listing-id }
            (merge listing { price-micro-stx: new-price-micro-stx })
        )
        (ok true)
    )
)

(define-public (deactivate-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
        (asserts! (is-eq (get seller listing) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active listing) ERR-LISTING-NOT-ACTIVE)

        (map-set listings
            { listing-id: listing-id }
            (merge listing { is-active: false })
        )
        (ok true)
    )
)

(define-public (reactivate-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
        (asserts! (is-eq (get seller listing) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-active listing)) ERR-LISTING-NOT-ACTIVE)
        (asserts! (not (get is-sold listing)) ERR-LISTING-ALREADY-SOLD)

        (map-set listings
            { listing-id: listing-id }
            (merge listing { is-active: true })
        )
        (ok true)
    )
)

;; ===============================================
;; PUBLIC FUNCTIONS - PURCHASING
;; ===============================================

(define-public (purchase-textbook (listing-id uint))
    (let (
        (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
        (price (get price-micro-stx listing))
        (seller (get seller listing))
        (platform-fee (calculate-platform-fee price))
        (seller-payment (- price platform-fee))
        (transaction-id (var-get next-transaction-id))
    )
        (asserts! (get is-active listing) ERR-LISTING-NOT-ACTIVE)
        (asserts! (not (get is-sold listing)) ERR-LISTING-ALREADY-SOLD)
        (asserts! (not (is-eq seller tx-sender)) ERR-CANNOT-BUY-OWN-LISTING)

        ;; Transfer payment to seller
        (unwrap! (stx-transfer? seller-payment tx-sender seller) ERR-TRANSFER-FAILED)

        ;; Transfer platform fee to contract
        (unwrap! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER) ERR-TRANSFER-FAILED)

        ;; Update listing as sold
        (map-set listings
            { listing-id: listing-id }
            (merge listing {
                is-active: false,
                is-sold: true,
                sold-at: (some stacks-block-height),
                buyer: (some tx-sender)
            })
        )

        ;; Record transaction
        (map-set transactions
            { transaction-id: transaction-id }
            {
                listing-id: listing-id,
                seller: seller,
                buyer: tx-sender,
                price-paid: price,
                platform-fee-paid: platform-fee,
                completed-at: stacks-block-height,
                rating-given: none
            }
        )

        ;; Update statistics
        (var-set next-transaction-id (+ transaction-id u1))
        (var-set platform-fee-collected (+ (var-get platform-fee-collected) platform-fee))
        (var-set total-volume (+ (var-get total-volume) price))

        ;; Update user profiles
        (update-user-profile-on-purchase tx-sender seller)

        (ok transaction-id)
    )
)

;; ===============================================
;; PUBLIC FUNCTIONS - MESSAGING
;; ===============================================

(define-public (send-message
    (listing-id uint)
    (recipient principal)
    (content (string-ascii 512))
)
    (let (
        (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
        (message-id (var-get next-message-id))
    )
        ;; Verify sender is either buyer or seller of the listing
        (asserts! (or
            (is-eq tx-sender (get seller listing))
            (and (is-some (get buyer listing)) (is-eq tx-sender (unwrap-panic (get buyer listing))))
        ) ERR-NOT-AUTHORIZED)

        (map-set messages
            { message-id: message-id }
            {
                listing-id: listing-id,
                sender: tx-sender,
                recipient: recipient,
                content: content,
                timestamp: stacks-block-height,
                is-read: false
            }
        )

        (var-set next-message-id (+ message-id u1))
        (ok message-id)
    )
)

(define-public (mark-message-read (message-id uint))
    (let ((message (unwrap! (map-get? messages { message-id: message-id }) ERR-MESSAGE-NOT-FOUND)))
        (asserts! (is-eq (get recipient message) tx-sender) ERR-NOT-AUTHORIZED)

        (map-set messages
            { message-id: message-id }
            (merge message { is-read: true })
        )
        (ok true)
    )
)

;; ===============================================
;; PUBLIC FUNCTIONS - RATINGS
;; ===============================================

(define-public (rate-transaction (transaction-id uint) (rating uint))
    (let ((transaction (unwrap! (map-get? transactions { transaction-id: transaction-id }) ERR-LISTING-NOT-FOUND)))
        (asserts! (is-eq (get buyer transaction) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (get rating-given transaction)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-CONDITION)

        (map-set transactions
            { transaction-id: transaction-id }
            (merge transaction { rating-given: (some rating) })
        )

        ;; Update seller's rating
        (let ((seller (get seller transaction))
              (seller-profile (default-to
                { total-listings: u0, total-purchases: u0, total-sales: u0,
                  average-rating: u0, total-ratings: u0, reputation-score: u0,
                  joined-at: stacks-block-height }
                (map-get? user-profiles { user: seller }))))
            (let ((new-total-ratings (+ (get total-ratings seller-profile) u1))
                  (new-rating-sum (+ (* (get average-rating seller-profile) (get total-ratings seller-profile)) rating)))
                (map-set user-profiles
                    { user: seller }
                    (merge seller-profile {
                        average-rating: (/ new-rating-sum new-total-ratings),
                        total-ratings: new-total-ratings,
                        reputation-score: (+ (get reputation-score seller-profile) rating)
                    })
                )
            )
        )
        (ok true)
    )
)

;; ===============================================
;; READ-ONLY FUNCTIONS
;; ===============================================

(define-read-only (get-listing (listing-id uint))
    (map-get? listings { listing-id: listing-id })
)

(define-read-only (get-message (message-id uint))
    (map-get? messages { message-id: message-id })
)

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles { user: user })
)

(define-read-only (get-transaction (transaction-id uint))
    (map-get? transactions { transaction-id: transaction-id })
)

(define-read-only (get-platform-stats)
    {
        total-listings: (- (var-get next-listing-id) u1),
        total-messages: (- (var-get next-message-id) u1),
        total-transactions: (- (var-get next-transaction-id) u1),
        platform-fee-collected: (var-get platform-fee-collected),
        total-volume: (var-get total-volume)
    }
)

(define-read-only (calculate-listing-fee (price uint))
    (calculate-platform-fee price)
)

;; ===============================================
;; ADMIN FUNCTIONS
;; ===============================================

(define-public (withdraw-platform-fees)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (let ((balance (var-get platform-fee-collected)))
            (var-set platform-fee-collected u0)
            (stx-transfer? balance (as-contract tx-sender) CONTRACT-OWNER)
        )
    )
)
