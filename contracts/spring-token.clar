;; balance mappings
(define-map balances
  ((owner principal))
  ((balance int)))

;; allowance mappings
(define-map allowances
  ((spender principal) (owner principal))
  ((allowance int)))

;; total supply
(define-data-var total-supply int 0)

;; Function to fetch balance of a specified account
(define (balance-of (account principal))
  (default-to 0
    (get balance
      (fetch-entry balances
        ((owner account))))))

;; Function to get allowance of a spender
(define (allowance-of (spender principal) (owner principal))
  (default-to 0
    (get allowance
      (fetch-entry allowances
        ((owner owner) (spender spender))))))

;; Function to decrease allowance of a spender
(define (decrease-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (or (> amount allowance) (<= amount 0))
      'true
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (- allowance amount))))
        'true))))

;; Function to increase allowance of a spender
(define (increase-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (<= amount 0)
      'false
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (+ allowance amount))))
        'true))))

;; Function to mint new tokens to specified account.
(define (mint! (account principal) (amount int))
  (if (<= amount 0)
    (err 'false)
    (let ((balance (balance-of account)))
      (begin
        (set-var! total-supply (+ (fetch-var total-supply) amount))
        (set-entry! balances
          ((owner account))
          ((balance (+ balance amount))))
        (ok amount)))))

;; Function to transfers tokens to a specified recipient.
(define (transfer! (sender principal) (recipient principal) (amount int))
  (if (eq? sender recipient)
    'false
    (let ((sender-balance (balance-of sender)))
    (let ((recipient-balance (balance-of recipient)))
    (if (or (> amount sender-balance) (<= amount 0))
     'false
      (begin
        (set-entry! balances
          ((owner sender))
          ((balance (- sender-balance amount))))
        (set-entry! balances
          ((owner recipient))
          ((balance (+ amount recipient-balance))))
        'true))))))

;; Function to get total supply of SpringTokens
(define-public (get-total-supply)
  (let ((ts (fetch-var total-supply)))
    (ok ts)))
  
;; Give the allowance for a given spender
(define-public (approve (spender principal) (amount int))
  (if (and (> amount 0) (increase-allowance! spender tx-sender amount))
    (ok amount)
    (err 'false)))

;; Revoke a given spender
(define-public (revoke (spender principal))
  (let ((allowance (allowance-of spender tx-sender)))
    (if (and (> allowance 0) (decrease-allowance! spender tx-sender allowance))
      (ok 'true)
      (err 'false))))

;; Transfers tokens to a specified principal.
(define-public (transfer (recipient principal) (amount int))
  (if (transfer! tx-sender recipient amount)
    (ok amount)
    (err 'false)))

;; Transfers tokens to a specified recipient, by a allowed spender
(define-public (transfer-from (owner principal) (recipient principal) (amount int))
  (let ((allowance (allowance-of tx-sender owner)))
    (if (or (> amount allowance) (<= amount 0))
      (err 'false)
      (if (and (transfer! owner recipient amount) (decrease-allowance! tx-sender owner amount))
        (ok amount)
        (err 'false)))))