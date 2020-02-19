// Tests to be written here
use super::*;
use crate::{Error, mock::*};
use frame_support::{assert_ok, assert_noop};


#[inline]
fn determine_address(account: &u64, block_numer: u64, salt: u64) -> u64 {
	DummyAddressGenerator::multi_sig_address(account, block_numer, salt)
}

#[test]
fn it_works_for_default_value() {
	new_test_ext().execute_with(|| {
		// Just a dummy test for the dummy funtion `do_something`
		// calling the `do_something` function with a value 42
		assert_ok!(Multisig::create_multisig_wallet(Origin::signed(1), vec![1, 2, 3], Percent::from_percent(50)));
		let multisig_wallet =  determine_address(&1, System::block_number(), System::account_nonce(&1));
		// asserting that the stored value is equal to what we stored
		assert_eq!(Multisig::multi_sig(&multisig_wallet), Some(MultiSigConfig {
			members: vec![1, 2, 3],
			threshold: Percent::from_percent(50),
			current_proposals: vec![]
		}));
		let call = Box::new(mock::Call::Multisig(MultiSigCall::add_member(4)));
		let call_hash = call_hash(call.clone());
		// proposal that `add account 4 as a member` brought up by account 1
		assert_ok!(Multisig::propose(Origin::signed(1), multisig_wallet, call.clone()));
		assert_eq!(Multisig::proposals(&call_hash), Some(ProposalStatus {
			multisig_wallet: multisig_wallet,
			approved_members: vec![1]
		}));

		assert_eq!(Multisig::multi_sig(&multisig_wallet), Some(MultiSigConfig {
			members: vec![1, 2, 3],
			threshold: Percent::from_percent(50),
			current_proposals: vec![call_hash]
		}));

		// the same one brought up by account 2
		assert_ok!(Multisig::propose(Origin::signed(2), multisig_wallet, call.clone()));
		// total active members are [1 ,2, 3] and the thresold is 50%
		// if account 1 and 2 have propoosed the same one,
		// this proposal should be executed automatically
		// and after executed, this proposal should be removed
		assert_eq!(Multisig::multi_sig(&multisig_wallet), Some(MultiSigConfig {
			members: vec![1, 2, 3, 4],
			threshold: Percent::from_percent(50),
			current_proposals: vec![]
		}));

		assert_eq!(Multisig::proposals(&call_hash), None);
	});


}
