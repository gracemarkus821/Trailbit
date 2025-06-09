(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-trail-inactive (err u106))

(define-fungible-token trailbit)

(define-data-var token-name (string-ascii 32) "Trailbit")
(define-data-var token-symbol (string-ascii 10) "TRAIL")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)

(define-map trails
  { trail-id: uint }
  {
    name: (string-ascii 64),
    location: (string-ascii 128),
    difficulty: uint,
    creator: principal,
    reward-pool: uint,
    discovery-reward: uint,
    maintenance-reward: uint,
    created-at: uint,
    active: bool,
    total-visits: uint
  }
)

(define-map trail-visits
  { trail-id: uint, visitor: principal }
  {
    visit-count: uint,
    last-visit: uint,
    total-earned: uint
  }
)

(define-map trail-maintenance
  { trail-id: uint, maintainer: principal }
  {
    maintenance-count: uint,
    last-maintenance: uint,
    total-earned: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    trails-created: uint,
    trails-visited: uint,
    maintenance-performed: uint,
    total-earned: uint
  }
)

(define-data-var next-trail-id uint u1)
(define-data-var platform-fee uint u50)

(define-public (create-trail (name (string-ascii 64)) (location (string-ascii 128)) (difficulty uint) (reward-pool uint) (discovery-reward uint) (maintenance-reward uint))
  (let
    (
      (trail-id (var-get next-trail-id))
      (current-block stacks-block-height)
    )
    (asserts! (> reward-pool u0) err-invalid-amount)
    (asserts! (> discovery-reward u0) err-invalid-amount)
    (asserts! (> maintenance-reward u0) err-invalid-amount)
    (asserts! (<= difficulty u5) err-invalid-amount)
    
    (try! (ft-transfer? trailbit reward-pool tx-sender (as-contract tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      {
        name: name,
        location: location,
        difficulty: difficulty,
        creator: tx-sender,
        reward-pool: reward-pool,
        discovery-reward: discovery-reward,
        maintenance-reward: maintenance-reward,
        created-at: current-block,
        active: true,
        total-visits: u0
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        { trails-created: (+ (get trails-created (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1) }
      )
    )
    
    (var-set next-trail-id (+ trail-id u1))
    (ok trail-id)
  )
)

(define-public (visit-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (visitor-data (default-to { visit-count: u0, last-visit: u0, total-earned: u0 } (map-get? trail-visits { trail-id: trail-id, visitor: tx-sender })))
      (reward-amount (get discovery-reward trail))
    )
    (asserts! (get active trail) err-trail-inactive)
    (asserts! (>= (get reward-pool trail) reward-amount) err-insufficient-funds)
    (asserts! (> (- current-block (get last-visit visitor-data)) u144) err-unauthorized)
    
    (try! (as-contract (ft-transfer? trailbit reward-amount tx-sender tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (- (get reward-pool trail) reward-amount),
        total-visits: (+ (get total-visits trail) u1)
      })
    )
    
    (map-set trail-visits
      { trail-id: trail-id, visitor: tx-sender }
      {
        visit-count: (+ (get visit-count visitor-data) u1),
        last-visit: current-block,
        total-earned: (+ (get total-earned visitor-data) reward-amount)
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        {
          trails-visited: (+ (get trails-visited (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1),
          total-earned: (+ (get total-earned (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) reward-amount)
        }
      )
    )
    
    (ok reward-amount)
  )
)

(define-public (maintain-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
      (current-block stacks-block-height)
      (maintainer-data (default-to { maintenance-count: u0, last-maintenance: u0, total-earned: u0 } (map-get? trail-maintenance { trail-id: trail-id, maintainer: tx-sender })))
      (reward-amount (get maintenance-reward trail))
    )
    (asserts! (get active trail) err-trail-inactive)
    (asserts! (>= (get reward-pool trail) reward-amount) err-insufficient-funds)
    (asserts! (> (- current-block (get last-maintenance maintainer-data)) u1008) err-unauthorized)
    
    (try! (as-contract (ft-transfer? trailbit reward-amount tx-sender tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (- (get reward-pool trail) reward-amount)
      })
    )
    
    (map-set trail-maintenance
      { trail-id: trail-id, maintainer: tx-sender }
      {
        maintenance-count: (+ (get maintenance-count maintainer-data) u1),
        last-maintenance: current-block,
        total-earned: (+ (get total-earned maintainer-data) reward-amount)
      }
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to
          { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 }
          (map-get? user-stats { user: tx-sender })
        )
        {
          maintenance-performed: (+ (get maintenance-performed (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) u1),
          total-earned: (+ (get total-earned (default-to { trails-created: u0, trails-visited: u0, maintenance-performed: u0, total-earned: u0 } (map-get? user-stats { user: tx-sender }))) reward-amount)
        }
      )
    )
    
    (ok reward-amount)
  )
)

(define-public (fund-trail (trail-id uint) (amount uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active trail) err-trail-inactive)
    
    (try! (ft-transfer? trailbit amount tx-sender (as-contract tx-sender)))
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail {
        reward-pool: (+ (get reward-pool trail) amount)
      })
    )
    
    (ok true)
  )
)

(define-public (deactivate-trail (trail-id uint))
  (let
    (
      (trail (unwrap! (map-get? trails { trail-id: trail-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator trail)) err-unauthorized)
    
    (map-set trails
      { trail-id: trail-id }
      (merge trail { active: false })
    )
    
    (ok true)
  )
)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (ft-mint? trailbit amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (ft-transfer? trailbit amount sender recipient)
  )
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance trailbit account))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply trailbit))
)

(define-read-only (get-name)
  (ok (var-get token-name))
)

(define-read-only (get-symbol)
  (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
  (ok (var-get token-decimals))
)

(define-read-only (get-token-uri)
  (ok none)
)

(define-read-only (get-trail (trail-id uint))
  (map-get? trails { trail-id: trail-id })
)

(define-read-only (get-trail-visits (trail-id uint) (visitor principal))
  (map-get? trail-visits { trail-id: trail-id, visitor: visitor })
)

(define-read-only (get-trail-maintenance (trail-id uint) (maintainer principal))
  (map-get? trail-maintenance { trail-id: trail-id, maintainer: maintainer })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-next-trail-id)
  (var-get next-trail-id)
)

(mint u1000000000000 contract-owner)