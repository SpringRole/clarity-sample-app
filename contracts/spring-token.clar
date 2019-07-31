;; mappings
(define-map balances
  ((owner principal))
  ((balance int)))

(define-map allowances
  ((spender principal) (owner principal))
  ((allowance int)))

;; variable 
(define-data-var total-supply int 0)

;; private functions

(define (allowance-of (spender principal) (owner principal))
  (default-to 0
    (get allowance
         (fetch-entry allowances ((owner owner)
                                 (spender spender))))))

(define (decrease-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (or (> amount allowance) (<= amount 0))
      'true
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (- allowance amount))))
        'true))))

(define (increase-allowance! (spender principal) (owner principal) (amount int))
  (let ((allowance (allowance-of spender owner)))
    (if (<= amount 0)
      'false
      (begin
        (set-entry! allowances
          ((spender spender) (owner owner))
          ((allowance (+ allowance amount))))
        'true))))

(define (balance-of (account principal))
  (default-to 0
    (get balance
         (fetch-entry balances ((owner account))))))
         
;; Mint new tokens.
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

;; Transfers tokens to a specified principal.
(define (transfer! (sender principal) (recipient principal) (amount int))
  (if (eq? sender recipient)
    (err 'false)
    (let ((sender-balance (balance-of sender)))
    (let ((recipient-balance (balance-of recipient)))
    (if (or (> amount sender-balance) (<= amount 0))
      (err 'false)
      (begin
        (set-entry! balances
          ((owner sender))
          ((balance (- sender-balance amount))))
        (set-entry! balances
                    ((owner recipient))
                    ((balance (+ amount recipient-balance))))
         (ok 'true)))))))
       
;; Public functions

(define-public (get-total-supply)
(let ((ts (fetch-var total-supply)))
  (ok ts)))
  
;; Update the allowance for a given spender
(define-public (approve (spender principal) (amount int))
  (if (and (> amount 0)
           (increase-allowance! spender tx-sender amount))
      (ok amount)
      (err 'false)))

;; Revoke a given spender
(define-public (revoke (spender principal))
  (let ((allowance (allowance-of spender tx-sender)))
    (if (and (> allowance 0)
             (decrease-allowance! spender tx-sender allowance))
        (ok 0)
        (err 'false))))

;; Transfers tokens to a specified principal.
(define-public (transfer (recipient principal) (amount int))
  (if (transfer! tx-sender recipient amount)
      (ok amount)
      (err 'false)))

;; Transfers tokens to a specified principal, performed by a spender
(define-public (transfer-from (owner principal) (recipient principal) (amount int))
  (let ((allowance (allowance-of tx-sender owner)))
    (if (or (> amount allowance) (<= amount 0))
      (err 'false)
      (if (and
           (transfer! owner recipient amount)
           (decrease-allowance! tx-sender owner amount))
       (ok amount)
       (err 'false)))))