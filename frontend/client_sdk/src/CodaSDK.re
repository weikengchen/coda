type publicKey = string;
type privateKey = string;
type globalSlot = string;
type uint64 = string;

// Max uint32
let defaultValidUntil = "4294967295";

[@genType]
type keypair = {
  privateKey,
  publicKey,
};

[@genType]
type signature = {
  field: string,
  scalar: string,
};

[@genType]
type signed('signable) = {
  publicKey,
  signature,
  payload: 'signable,
};

type stakeDelegation = {
  [@bs.as "to"]
  to_: publicKey,
  from: publicKey,
  fee: uint64,
  nonce: int,
  memo: option(string),
  validUntil: option(globalSlot),
};

type payment = {
  [@bs.as "to"]
  to_: publicKey,
  from: publicKey,
  fee: uint64,
  amount: uint64,
  nonce: int,
  memo: option(string),
  validUntil: option(globalSlot),
};

type codaSDK;
[@bs.module "./client_sdk.bc.js"] external codaSDK: codaSDK = "codaSDK";

[@bs.send] external genKeys: (codaSDK, unit) => keypair = "genKeys";
/**
  * Generates a public/private keypair
 */
[@genType]
let genKeys = () => genKeys(codaSDK, ());

[@bs.send]
external signString: (codaSDK, privateKey, string) => signature = "signString";
/**
  * Signs an arbitrary message
  *
  * @param key - The keypair used to sign the message
  * @param message - An arbitrary string message to be signed
  * @returns A signed message
 */
[@genType]
let signMessage =
  (. message: string, key: keypair) => {
    publicKey: key.publicKey,
    signature: signString(codaSDK, key.privateKey, message),
    payload: message,
  };

[@bs.send]
external verifyStringSignature: (codaSDK, signature, publicKey, string) => bool =
  "verifyStringSignature";
/**
  * Verifies that a signature matches a message.
  *
  * @param signedMessage - A signed message
  * @returns True if the `signedMessage` contains a valid signature matching
  * the message and publicKey.
 */
[@genType]
let verifyMessage =
  (. signedMessage: signed(string)) => {
    verifyStringSignature(
      codaSDK,
      signedMessage.signature,
      signedMessage.publicKey,
      signedMessage.payload,
    );
  };

/**
 * Same as Option.value, used to avoid bringing in bs-platform as a dep.
 * This is a simplified version that should not be used on nested options.
 * Would be fixed by https://github.com/BuckleScript/bucklescript/pull/2171
 */
let value = (~default: 'a, option: option('a)): 'a => {
  let go = () => [%bs.raw
    {|
    function value($$default, opt) {
      if (opt !== undefined) {
        return opt;
      } else {
        return $$default;
      }
    }
  |}
  ];
  (go())(. default, option);
};

type common_payload_js = {
  .
  "fee": string,
  "nonce": string,
  "validUntil": string,
  "memo": string,
};

type payment_js = {
  .
  "common": common_payload_js,
  "paymentPayload": {
    .
    "receiver": publicKey,
    "amount": string,
  },
};

type stake_delegation_js = {
  .
  "common": common_payload_js,
  "new_delegate": publicKey,
};

type signed_js = {
  .
  "stakeDelegation": Js.Undefined.t(stake_delegation_js),
  "payment": Js.Undefined.t(payment_js),
  "sender": publicKey,
  "signature": signature,
};

[@bs.send]
external signPayment: (codaSDK, privateKey, payment_js) => signed_js =
  "signPayment";
/**
  * Signs a payment transaction using a private key.
  *
  * This type of transaction allows a user to transfer funds from one account
  * to another over the network.
  *
  * @param payment - An object describing the payment
  * @param key - The keypair used to sign the transaction
  * @returns A signed payment transaction
 */
[@genType]
let signPayment =
  (. payment: payment, key: keypair) => {
    let memo = value(~default="", payment.memo);
    let validUntil = value(~default=defaultValidUntil, payment.validUntil);
    {
      publicKey: key.publicKey,
      payload: {
        ...payment,
        // Set missing values so that the signature can be checked without guessing defaults.
        memo: Some(memo),
        validUntil: Some(validUntil),
      },
      signature:
        signPayment(
          codaSDK,
          key.privateKey,
          {
            "common": {
              "fee": payment.fee,
              "nonce": string_of_int(payment.nonce),
              "validUntil": validUntil,
              "memo": memo,
            },
            "paymentPayload": {
              "receiver": payment.to_,
              "amount": payment.amount,
            },
          },
        )##signature,
    };
  };

[@bs.send]
external signStakeDelegation:
  (codaSDK, privateKey, stake_delegation_js) => signed_js =
  "signStakeDelegation";

/**
  * Signs a stake delegation transaction using a private key.
  *
  * This type of transaction allows a user to delegate their
  * funds from one account to another for use in staking. The
  * account that is delegated to is then considered as having these
  * funds when determininng whether it can produce a block in a given slot.
  *
  * @param stakeDelegation - An object describing the stake delegation
  * @param key - The keypair used to sign the transaction
  * @returns A signed stake delegation
 */
[@genType]
let signStakeDelegation =
  (. stakeDelegation: stakeDelegation, key: keypair) => {
    let memo = value(~default="", stakeDelegation.memo);
    let validUntil = value(~default=defaultValidUntil, stakeDelegation.validUntil);
    {
      publicKey: key.publicKey,
      payload: {
        ...stakeDelegation,
        // Set missing values so that the signature can be checked without guessing defaults.
        memo: Some(memo),
        validUntil: Some(validUntil),
      },
      signature:
        signStakeDelegation(
          codaSDK,
          key.privateKey,
          {
            "common": {
              "fee": stakeDelegation.fee,
              "nonce": string_of_int(stakeDelegation.nonce),
              "validUntil": validUntil,
              "memo": memo,
            },
            "new_delegate": stakeDelegation.to_,
          },
        )##signature,
    };
  };