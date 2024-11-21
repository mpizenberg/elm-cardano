use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/certificate.{
  Certificate, RegisterDelegateRepresentative, UnregisterDelegateRepresentative,
}
use cardano/governance.{DelegateRepresentative, Voter}
use cardano/transaction.{OutputReference, Transaction}

validator drep_voting {
  spend(
    datum: Option<VerificationKeyHash>,
    _redeemer: Data,
    _own_ref: OutputReference,
    self: Transaction,
  ) {
    expect Some(owner) = datum
    list.has(self.extra_signatories, owner)?
  }

  publish(_redeemer: Data, certificate: Certificate, self: Transaction) {
    when certificate is {
      RegisterDelegateRepresentative { delegate_representative, deposit } ->
        // todo @"register"
        True
      UnregisterDelegateRepresentative { delegate_representative, refund } ->
        // todo @"unregister"
        True
      _ -> fail
    }
  }

  vote(_redeemer: Data, voter: Voter, self: Transaction) {
    when voter is {
      DelegateRepresentative(cred) ->
        // todo @"drep"
        True
      _ -> fail
    }
  }

  else(_) {
    fail
  }
}
