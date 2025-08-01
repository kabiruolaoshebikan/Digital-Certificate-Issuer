;; =================================================================
;; DIGITAL CERTIFICATE ISSUER SYSTEM
;; =================================================================
;; A comprehensive system for issuing, managing, and verifying
;; digital certificates for courses, achievements, and skills.
;; =================================================================

;; =================================================================
;; CONSTANTS AND ERROR CODES
;; =================================================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CERTIFICATE-NOT-FOUND (err u101))
(define-constant ERR-CERTIFICATE-REVOKED (err u102))
(define-constant ERR-INVALID-ISSUER (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-TEMPLATE (err u105))
(define-constant ERR-INVALID-RECIPIENT (err u106))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u107))
(define-constant ERR-ISSUER-NOT-VERIFIED (err u108))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-CERTIFICATE-ID u999999999)
(define-constant MAX-TEMPLATE-ID u999999)

;; =================================================================
;; DATA VARIABLES
;; =================================================================

;; Global counters
(define-data-var certificate-id-nonce uint u0)
(define-data-var template-id-nonce uint u0)
(define-data-var total-certificates-issued uint u0)

;; Contract settings
(define-data-var contract-paused bool false)

;; =================================================================
;; DATA MAPS
;; =================================================================

;; Verified issuers registry
(define-map verified-issuers
    principal
    {
        name: (string-ascii 100),
        description: (string-ascii 500),
        website: (string-ascii 200),
        verified-at: uint,
        verified-by: principal,
        active: bool
    }
)

;; Certificate templates
(define-map certificate-templates
    uint ;; template-id
    {
        name: (string-ascii 100),
        description: (string-ascii 500),
        issuer: principal,
        category: (string-ascii 50),
        metadata-uri: (string-ascii 500),
        created-at: uint,
        active: bool,
        requirements: (string-ascii 1000)
    }
)

;; Issued certificates
(define-map certificates
    uint ;; certificate-id
    {
        template-id: uint,
        recipient: principal,
        issuer: principal,
        issued-at: uint,
        expires-at: (optional uint),
        metadata-uri: (string-ascii 500),
        grade: (optional (string-ascii 10)),
        credits: (optional uint),
        revoked: bool,
        revoked-at: (optional uint),
        revoked-reason: (optional (string-ascii 200))
    }
)

;; Certificate ownership tracking
(define-map certificate-ownership
    {owner: principal, certificate-id: uint}
    bool
)

;; Recipient certificate count
(define-map recipient-certificate-count
    principal
    uint
)

;; Issuer statistics
(define-map issuer-stats
    principal
    {
        certificates-issued: uint,
        templates-created: uint,
        last-activity: uint
    }
)

;; =================================================================
;; AUTHORIZATION FUNCTIONS
;; =================================================================

(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-verified-issuer (issuer principal))
    (match (map-get? verified-issuers issuer)
        issuer-data (get active issuer-data)
        false
    )
)

(define-private (is-certificate-owner (certificate-id uint) (user principal))
    (default-to false
        (map-get? certificate-ownership {owner: user, certificate-id: certificate-id})
    )
)

;; =================================================================
;; ISSUER MANAGEMENT
;; =================================================================

;; Register a new verified issuer (only contract owner)
(define-public (register-issuer
    (issuer principal)
    (name (string-ascii 100))
    (description (string-ascii 500))
    (website (string-ascii 200)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? verified-issuers issuer)) ERR-ALREADY-EXISTS)

        (map-set verified-issuers issuer {
            name: name,
            description: description,
            website: website,
            verified-at: stacks-block-height,
            verified-by: tx-sender,
            active: true
        })

        (ok issuer)
    )
)

;; Deactivate an issuer
(define-public (deactivate-issuer (issuer principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)

        (match (map-get? verified-issuers issuer)
            issuer-data (begin
                (map-set verified-issuers issuer
                    (merge issuer-data {active: false}))
                (ok true)
            )
            ERR-INVALID-ISSUER
        )
    )
)

;; =================================================================
;; TEMPLATE MANAGEMENT
;; =================================================================

;; Create a certificate template
(define-public (create-template
    (name (string-ascii 100))
    (description (string-ascii 500))
    (category (string-ascii 50))
    (metadata-uri (string-ascii 500))
    (requirements (string-ascii 1000)))
    (let (
        (template-id (+ (var-get template-id-nonce) u1))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-verified-issuer tx-sender) ERR-ISSUER-NOT-VERIFIED)
        (asserts! (< template-id MAX-TEMPLATE-ID) ERR-INVALID-TEMPLATE)

        (map-set certificate-templates template-id {
            name: name,
            description: description,
            issuer: tx-sender,
            category: category,
            metadata-uri: metadata-uri,
            created-at: stacks-block-height,
            active: true,
            requirements: requirements
        })

        ;; Update counters and stats
        (var-set template-id-nonce template-id)
        (update-issuer-template-count tx-sender)

        (ok template-id)
    )
)

;; Deactivate a template
(define-public (deactivate-template (template-id uint))
    (match (map-get? certificate-templates template-id)
        template-data (begin
            (asserts! (is-eq tx-sender (get issuer template-data)) ERR-NOT-AUTHORIZED)

            (map-set certificate-templates template-id
                (merge template-data {active: false}))
            (ok true)
        )
        ERR-TEMPLATE-NOT-FOUND
    )
)

;; =================================================================
;; CERTIFICATE ISSUANCE
;; =================================================================

;; Issue a certificate
(define-public (issue-certificate
    (template-id uint)
    (recipient principal)
    (metadata-uri (string-ascii 500))
    (expires-at (optional uint))
    (grade (optional (string-ascii 10)))
    (credits (optional uint)))
    (let (
        (certificate-id (+ (var-get certificate-id-nonce) u1))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-verified-issuer tx-sender) ERR-ISSUER-NOT-VERIFIED)
        (asserts! (< certificate-id MAX-CERTIFICATE-ID) ERR-INVALID-RECIPIENT)

        ;; Validate template exists and is active
        (match (map-get? certificate-templates template-id)
            template-data (begin
                (asserts! (get active template-data) ERR-INVALID-TEMPLATE)
                (asserts! (is-eq tx-sender (get issuer template-data)) ERR-NOT-AUTHORIZED)

                ;; Create certificate
                (map-set certificates certificate-id {
                    template-id: template-id,
                    recipient: recipient,
                    issuer: tx-sender,
                    issued-at: stacks-block-height,
                    expires-at: expires-at,
                    metadata-uri: metadata-uri,
                    grade: grade,
                    credits: credits,
                    revoked: false,
                    revoked-at: none,
                    revoked-reason: none
                })

                ;; Set ownership
                (map-set certificate-ownership
                    {owner: recipient, certificate-id: certificate-id} true)

                ;; Update counters
                (var-set certificate-id-nonce certificate-id)
                (var-set total-certificates-issued
                    (+ (var-get total-certificates-issued) u1))

                ;; Update recipient count
                (update-recipient-certificate-count recipient)

                ;; Update issuer stats
                (update-issuer-certificate-count tx-sender)

                (ok certificate-id)
            )
            ERR-TEMPLATE-NOT-FOUND
        )
    )
)

;; Revoke a certificate
(define-public (revoke-certificate
    (certificate-id uint)
    (reason (string-ascii 200)))
    (match (map-get? certificates certificate-id)
        certificate-data (begin
            (asserts! (is-eq tx-sender (get issuer certificate-data)) ERR-NOT-AUTHORIZED)
            (asserts! (not (get revoked certificate-data)) ERR-CERTIFICATE-REVOKED)

            (map-set certificates certificate-id
                (merge certificate-data {
                    revoked: true,
                    revoked-at: (some stacks-block-height),
                    revoked-reason: (some reason)
                }))

            (ok true)
        )
        ERR-CERTIFICATE-NOT-FOUND
    )
)

;; =================================================================
;; VERIFICATION FUNCTIONS
;; =================================================================

;; Verify certificate validity
(define-read-only (verify-certificate (certificate-id uint))
    (match (map-get? certificates certificate-id)
        certificate-data (begin
            ;; Check if certificate is revoked
            (asserts! (not (get revoked certificate-data)) ERR-CERTIFICATE-REVOKED)

            ;; Check if certificate has expired
            (match (get expires-at certificate-data)
                expiry-block (asserts! (< stacks-block-height expiry-block)
                    (err u109)) ;; ERR-CERTIFICATE-EXPIRED
                true ;; No expiry set
            )

            ;; Verify issuer is still active
            (asserts! (is-verified-issuer (get issuer certificate-data))
                ERR-ISSUER-NOT-VERIFIED)

            (ok {
                valid: true,
                certificate: certificate-data,
                verified-at: stacks-block-height
            })
        )
        ERR-CERTIFICATE-NOT-FOUND
    )
)

;; Get certificate details
(define-read-only (get-certificate (certificate-id uint))
    (map-get? certificates certificate-id)
)

;; Get template details
(define-read-only (get-template (template-id uint))
    (map-get? certificate-templates template-id)
)

;; Get issuer information
(define-read-only (get-issuer-info (issuer principal))
    (map-get? verified-issuers issuer)
)

;; =================================================================
;; WALLET INTEGRATION FUNCTIONS
;; =================================================================

;; Get certificates owned by a principal
(define-read-only (get-certificates-by-owner (owner principal))
    (let (
        (cert-count (default-to u0
            (map-get? recipient-certificate-count owner)))
    )
        (ok {
            owner: owner,
            certificate-count: cert-count,
            query-block: stacks-block-height
        })
    )
)

;; Check if user owns a specific certificate
(define-read-only (owns-certificate (owner principal) (certificate-id uint))
    (is-certificate-owner certificate-id owner)
)

;; =================================================================
;; STATISTICS AND ANALYTICS
;; =================================================================

;; Get contract statistics
(define-read-only (get-contract-stats)
    (ok {
        total-certificates: (var-get total-certificates-issued),
        current-certificate-id: (var-get certificate-id-nonce),
        current-template-id: (var-get template-id-nonce),
        contract-paused: (var-get contract-paused),
        current-block: stacks-block-height
    })
)

;; Get issuer statistics
(define-read-only (get-issuer-stats (issuer principal))
    (map-get? issuer-stats issuer)
)

;; =================================================================
;; HELPER FUNCTIONS
;; =================================================================

;; Update recipient certificate count
(define-private (update-recipient-certificate-count (recipient principal))
    (let (
        (current-count (default-to u0
            (map-get? recipient-certificate-count recipient)))
    )
        (map-set recipient-certificate-count recipient (+ current-count u1))
    )
)

;; Update issuer certificate count
(define-private (update-issuer-certificate-count (issuer principal))
    (let (
        (current-stats (default-to
            {certificates-issued: u0, templates-created: u0, last-activity: u0}
            (map-get? issuer-stats issuer)))
    )
        (map-set issuer-stats issuer {
            certificates-issued: (+ (get certificates-issued current-stats) u1),
            templates-created: (get templates-created current-stats),
            last-activity: stacks-block-height
        })
    )
)

;; Update issuer template count
(define-private (update-issuer-template-count (issuer principal))
    (let (
        (current-stats (default-to
            {certificates-issued: u0, templates-created: u0, last-activity: u0}
            (map-get? issuer-stats issuer)))
    )
        (map-set issuer-stats issuer {
            certificates-issued: (get certificates-issued current-stats),
            templates-created: (+ (get templates-created current-stats) u1),
            last-activity: stacks-block-height
        })
    )
)

;; =================================================================
;; ADMIN FUNCTIONS
;; =================================================================

;; Pause/unpause contract
(define-public (set-contract-paused (paused bool))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (var-set contract-paused paused)
        (ok paused)
    )
)

;; Emergency revoke certificate (admin only)
(define-public (admin-revoke-certificate
    (certificate-id uint)
    (reason (string-ascii 200)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)

        (match (map-get? certificates certificate-id)
            certificate-data (begin
                (map-set certificates certificate-id
                    (merge certificate-data {
                        revoked: true,
                        revoked-at: (some stacks-block-height),
                        revoked-reason: (some reason)
                    }))
                (ok true)
            )
            ERR-CERTIFICATE-NOT-FOUND
        )
    )
)

;; =================================================================
;; CONTRACT INITIALIZATION
;; =================================================================

;; Initialize contract with default verified issuer (contract owner)
(map-set verified-issuers CONTRACT-OWNER {
    name: "System Administrator",
    description: "Default system administrator issuer",
    website: "https://system.admin",
    verified-at: stacks-block-height,
    verified-by: CONTRACT-OWNER,
    active: true
})

;; =================================================================
;; END OF CONTRACT
;; =================================================================
