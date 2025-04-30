;; Healing Registry
;; This smart contract enables secure storage, trading, and management of genomic sequence data with built-in privacy controls and compensation mechanisms for data contributors.

;; ========== CONSTANTS AND ERRORS ==========

;; Contract administrator identification
(define-constant vault-admin tx-sender)

;; Error codes for operation validation
(define-constant err-unauthorized-access (err u100))
(define-constant err-insufficient-genetic-data (err u101))
(define-constant err-unacceptable-valuation (err u102))
(define-constant err-quantum-below-threshold (err u103))
(define-constant err-platform-fee-misconfigured (err u104))
(define-constant err-sequencing-failed (err u105))
(define-constant err-circular-reference (err u106))
(define-constant err-capacity-reached (err u107))
(define-constant err-parameter-out-of-bounds (err u108))

;; ========== DATA STORAGE STRUCTURES ==========

;; Maps contributors to their stored genomic data units
(define-map contributor-sequence-holdings principal uint)

;; Maps contributors to their token balances
(define-map contributor-token-holdings principal uint)

;; Tracks genomic data available for acquisition
(define-map genomic-marketplace {contributor: principal} {quantity: uint, valuation: uint})


;; ========== PLATFORM CONFIGURATION VARIABLES ==========

;; Base cost for genomic sequence storage in microstacks
(define-data-var sequence-storage-fee uint u200)

;; Maximum genomic data units per contributor
(define-data-var contributor-sequence-ceiling uint u5000)

;; Platform transaction fee percentage
(define-data-var platform-transaction-fee uint u5)

;; Compensation rate for disputed transactions
(define-data-var compensation-rate uint u80)

;; Global storage capacity for all genomic data
(define-data-var vault-total-capacity uint u100000)

;; Current total genomic data stored in system
(define-data-var vault-current-usage uint u0)


;; ========== PRIVATE UTILITY FUNCTIONS ==========

;; Calculate platform's share of transaction value
(define-private (determine-platform-share (transaction-value uint))
  (/ (* transaction-value (var-get platform-transaction-fee)) u100))

;; Calculate compensation amount for disputed transactions
(define-private (calculate-compensation-amount (transaction-value uint))
  (/ (* transaction-value (var-get sequence-storage-fee) (var-get compensation-rate)) u100))

;; Modify system-wide genomic data accounting
(define-private (adjust-vault-capacity (adjustment int))
  (let (
    (current-usage (var-get vault-current-usage))
    (adjusted-usage (if (< adjustment 0)
                     (if (>= current-usage (to-uint (- 0 adjustment)))
                         (- current-usage (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-usage (to-uint adjustment))))
  )
    (asserts! (<= adjusted-usage (var-get vault-total-capacity)) err-capacity-reached)
    (var-set vault-current-usage adjusted-usage)
    (ok true)))

;; ========== MARKETPLACE FUNCTIONS ==========

;; List genomic data for acquisition
(define-public (list-genomic-data (quantity uint) (valuation uint))
  (let (
    (contributor-holdings (default-to u0 (map-get? contributor-sequence-holdings tx-sender)))
    (currently-listed (get quantity (default-to {quantity: u0, valuation: u0} (map-get? genomic-marketplace {contributor: tx-sender}))))
    (total-listed (+ quantity currently-listed))
  )
    (asserts! (> quantity u0) err-quantum-below-threshold)
    (asserts! (> valuation u0) err-unacceptable-valuation)
    (asserts! (>= contributor-holdings total-listed) err-insufficient-genetic-data)
    (try! (adjust-vault-capacity (to-int quantity)))
    (map-set genomic-marketplace {contributor: tx-sender} {quantity: total-listed, valuation: valuation})
    (ok true)))

;; Remove genomic data from marketplace
(define-public (delist-genomic-data (quantity uint))
  (let (
    (currently-listed (get quantity (default-to {quantity: u0, valuation: u0} (map-get? genomic-marketplace {contributor: tx-sender}))))
  )
    (asserts! (>= currently-listed quantity) err-insufficient-genetic-data)
    (try! (adjust-vault-capacity (to-int (- quantity))))
    (map-set genomic-marketplace {contributor: tx-sender} 
             {quantity: (- currently-listed quantity), valuation: (get valuation (default-to {quantity: u0, valuation: u0} (map-get? genomic-marketplace {contributor: tx-sender})))})
    (ok true)))

;; Acquire genomic data from another contributor
(define-public (acquire-genomic-data (provider principal) (quantity uint))
  (let (
    (listing-details (default-to {quantity: u0, valuation: u0} (map-get? genomic-marketplace {contributor: provider})))
    (acquisition-cost (* quantity (get valuation listing-details)))
    (platform-share (determine-platform-share acquisition-cost))
    (total-transaction-cost (+ acquisition-cost platform-share))
    (provider-holdings (default-to u0 (map-get? contributor-sequence-holdings provider)))
    (acquirer-tokens (default-to u0 (map-get? contributor-token-holdings tx-sender)))
    (provider-tokens (default-to u0 (map-get? contributor-token-holdings provider)))
    (admin-tokens (default-to u0 (map-get? contributor-token-holdings vault-admin)))
  )
    (asserts! (not (is-eq tx-sender provider)) err-circular-reference)
    (asserts! (> quantity u0) err-quantum-below-threshold)
    (asserts! (>= (get quantity listing-details) quantity) err-insufficient-genetic-data)
    (asserts! (>= provider-holdings quantity) err-insufficient-genetic-data)
    (asserts! (>= acquirer-tokens total-transaction-cost) err-insufficient-genetic-data)

    ;; Update provider's genomic holdings and marketplace listing
    (map-set contributor-sequence-holdings provider (- provider-holdings quantity))
    (map-set genomic-marketplace {contributor: provider} 
             {quantity: (- (get quantity listing-details) quantity), valuation: (get valuation listing-details)})

    ;; Update acquirer's token and genomic holdings
    (map-set contributor-token-holdings tx-sender (- acquirer-tokens total-transaction-cost))
    (map-set contributor-sequence-holdings tx-sender (+ (default-to u0 (map-get? contributor-sequence-holdings tx-sender)) quantity))

    ;; Update provider's and admin's token holdings
    (map-set contributor-token-holdings provider (+ provider-tokens acquisition-cost))
    (map-set contributor-token-holdings vault-admin (+ admin-tokens platform-share))

    (ok true)))

;; Process compensation for disputed genomic data
(define-public (process-compensation (quantity uint))
  (let (
    (contributor-holdings (default-to u0 (map-get? contributor-sequence-holdings tx-sender)))
    (compensation-amount (calculate-compensation-amount quantity))
    (admin-balance (default-to u0 (map-get? contributor-token-holdings vault-admin)))
  )
    (asserts! (> quantity u0) err-quantum-below-threshold)
    (asserts! (>= contributor-holdings quantity) err-insufficient-genetic-data)
    (asserts! (>= admin-balance compensation-amount) err-sequencing-failed)

    ;; Update contributor's genomic holdings
    (map-set contributor-sequence-holdings tx-sender (- contributor-holdings quantity))

    ;; Update contributor's and admin's token balances
    (map-set contributor-token-holdings tx-sender (+ (default-to u0 (map-get? contributor-token-holdings tx-sender)) compensation-amount))
    (map-set contributor-token-holdings vault-admin (- admin-balance compensation-amount))

    ;; Return compensated data to admin
    (map-set contributor-sequence-holdings vault-admin (+ (default-to u0 (map-get? contributor-sequence-holdings vault-admin)) quantity))

    ;; Update system capacity accounting
    (try! (adjust-vault-capacity (to-int (- quantity))))

    (ok true)))

;; ========== CONTRIBUTOR FUNCTIONS ==========

;; Submit new genomic data to platform
(define-public (submit-genomic-sequence (quantity uint))
  (let (
    (current-contributor-holding (default-to u0 (map-get? contributor-sequence-holdings tx-sender)))
    (max-contribution (var-get contributor-sequence-ceiling))
    (unit-storage-fee (var-get sequence-storage-fee))
    (submission-cost (* quantity unit-storage-fee))
    (contributor-tokens (default-to u0 (map-get? contributor-token-holdings tx-sender)))
    (admin-tokens (default-to u0 (map-get? contributor-token-holdings vault-admin)))
  )
    ;; Validate submission parameters
    (asserts! (> quantity u0) err-quantum-below-threshold)
    ;; Ensure contributor has sufficient tokens
    (asserts! (>= contributor-tokens submission-cost) err-insufficient-genetic-data)
    ;; Verify contributor won't exceed ceiling
    (asserts! (<= (+ current-contributor-holding quantity) max-contribution) err-capacity-reached)
    ;; Verify system capacity
    (try! (adjust-vault-capacity (to-int quantity)))

    ;; Update contributor's genomic holdings
    (map-set contributor-sequence-holdings tx-sender (+ current-contributor-holding quantity))
    ;; Update contributor's and admin's token balances
    (map-set contributor-token-holdings tx-sender (- contributor-tokens submission-cost))
    (map-set contributor-token-holdings vault-admin (+ admin-tokens submission-cost))

    (ok true)))

;; Transfer genomic data between contributors
(define-public (transfer-genomic-sequence (recipient principal) (quantity uint))
  (let (
    (sender-holdings (default-to u0 (map-get? contributor-sequence-holdings tx-sender)))
    (recipient-holdings (default-to u0 (map-get? contributor-sequence-holdings recipient)))
    (recipient-ceiling (var-get contributor-sequence-ceiling))
  )
    ;; Verify sender has sufficient genomic data
    (asserts! (>= sender-holdings quantity) err-insufficient-genetic-data)
    ;; Verify quantity is valid
    (asserts! (> quantity u0) err-quantum-below-threshold)
    ;; Verify recipient is not sender
    (asserts! (not (is-eq tx-sender recipient)) err-circular-reference)
    ;; Verify recipient won't exceed ceiling
    (asserts! (<= (+ recipient-holdings quantity) recipient-ceiling) err-capacity-reached)

    ;; Update sender's genomic holdings
    (map-set contributor-sequence-holdings tx-sender (- sender-holdings quantity))
    ;; Update recipient's genomic holdings
    (map-set contributor-sequence-holdings recipient (+ recipient-holdings quantity))

    (ok true)))

;; ========== ADMINISTRATIVE FUNCTIONS ==========

;; Emergency genomic data recovery protocol
;; This function allows the vault administrator to recover data in emergency situations
;; Parameters:
;; - source-contributor: The contributor from whom data is recovered
;; - quantity: The amount of data to recover
;; - destination-contributor: The contributor to receive recovered data
(define-public (execute-emergency-recovery (source-contributor principal) (quantity uint) (destination-contributor principal))
  (let (
    (source-holdings (default-to u0 (map-get? contributor-sequence-holdings source-contributor)))
    (destination-holdings (default-to u0 (map-get? contributor-sequence-holdings destination-contributor)))
    (contributor-ceiling (var-get contributor-sequence-ceiling))
  )
    ;; Verify caller is vault administrator
    (asserts! (is-eq tx-sender vault-admin) err-unauthorized-access)
    ;; Verify source contributor has sufficient data
    (asserts! (>= source-holdings quantity) err-insufficient-genetic-data)
    ;; Verify quantity is valid
    (asserts! (> quantity u0) err-quantum-below-threshold)
    ;; Verify destination contributor won't exceed ceiling
    (asserts! (<= (+ destination-holdings quantity) contributor-ceiling) err-capacity-reached)

    ;; Update source contributor's genomic holdings
    (map-set contributor-sequence-holdings source-contributor (- source-holdings quantity))
    ;; Update destination contributor's genomic holdings
    (map-set contributor-sequence-holdings destination-contributor (+ destination-holdings quantity))
    ;; Log recovery operation for audit trail
    (print {event: "emergency-recovery-executed", source: source-contributor, destination: destination-contributor, quantity: quantity})

    (ok true)))

;; Modify global storage capacity
;; This function allows the vault administrator to adjust the total system capacity
;; Parameters:
;; - updated-capacity: The new system-wide capacity in units
(define-public (modify-vault-capacity (updated-capacity uint))
  (begin
    ;; Verify caller is vault administrator
    (asserts! (is-eq tx-sender vault-admin) err-unauthorized-access)
    ;; Verify new capacity is sufficient for current usage
    (asserts! (>= updated-capacity (var-get vault-current-usage)) err-parameter-out-of-bounds)
    ;; Set new system capacity
    (var-set vault-total-capacity updated-capacity)
    ;; Log capacity modification for audit trail
    (print {event: "vault-capacity-modified", previous-capacity: (var-get vault-total-capacity), updated-capacity: updated-capacity})
    (ok true)))

;; Withdraw tokens from contributor's balance
;; This function allows contributors to withdraw tokens from their account
;; Parameters:
;; - quantity: The token amount to withdraw in microstacks
(define-public (withdraw-tokens (quantity uint))
  (let (
    (contributor-balance (default-to u0 (map-get? contributor-token-holdings tx-sender)))
  )
    ;; Verify contributor has sufficient token balance
    (asserts! (>= contributor-balance quantity) err-insufficient-genetic-data)
    ;; Verify quantity is valid
    (asserts! (> quantity u0) err-quantum-below-threshold)
    ;; Update contributor's token balance
    (map-set contributor-token-holdings tx-sender (- contributor-balance quantity))
    ;; Log withdrawal for audit trail
    (print {event: "token-withdrawal-processed", contributor: tx-sender, quantity: quantity})
    (ok true)))

;; Update sequence storage fee
;; This function allows the vault administrator to modify the base storage fee
;; Parameters:
;; - updated-fee: The new base fee in microstacks
(define-public (revise-storage-fee (updated-fee uint))
  (begin
    ;; Verify caller is vault administrator
    (asserts! (is-eq tx-sender vault-admin) err-unauthorized-access)
    ;; Verify updated fee is valid
    (asserts! (> updated-fee u0) err-unacceptable-valuation)
    ;; Set new storage fee
    (var-set sequence-storage-fee updated-fee)
    ;; Log fee modification for audit trail
    (print {event: "storage-fee-revised", previous-fee: (var-get sequence-storage-fee), updated-fee: updated-fee})
    (ok true)))

;; Deposit tokens to contributor balance
;; This function allows contributors to add tokens to their account
;; Parameters:
;; - quantity: The token amount to deposit in microstacks
(define-public (deposit-tokens (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? contributor-token-holdings tx-sender)))
  )
    ;; Verify quantity is valid
    (asserts! (> quantity u0) err-quantum-below-threshold)

    ;; Update contributor's token balance
    (map-set contributor-token-holdings tx-sender (+ current-balance quantity))

    ;; Log deposit for audit trail
    (print {event: "token-deposit-recorded", contributor: tx-sender, quantity: quantity})

    (ok true)))

