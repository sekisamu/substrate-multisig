#![cfg_attr(not(feature = "std"), no_std)]

/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references

/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/frame/example/src/lib.rs

use sp_std::{ prelude::*, marker::PhantomData, collections::btree_set::BTreeSet };
use frame_support::{
	decl_module, decl_storage, decl_event, decl_error, 
	dispatch, ensure, Parameter, RuntimeDebug, StorageValue,
	weights::{
		GetDispatchInfo, PaysFee, DispatchClass, ClassifyDispatch, Weight, WeighData,
		SimpleDispatchInfo,
	},
	traits::{Currency, ReservableCurrency, Get},
};
use sp_core::crypto::UncheckedFrom;

use sp_arithmetic::{ PerThing, Percent };
use sp_runtime::{ DispatchResult, traits::{ Hash, Dispatchable, SaturatedConversion }};
use frame_system::{ self as system, ensure_root, ensure_signed };
use codec::{Encode, Decode};


#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;


pub trait AddressDeterminator<AccountId, BlockNumber, Index> {
	fn multi_sig_address(creator: &AccountId, block_number: BlockNumber, salt: Index) -> AccountId;
}

/// the configuration of a multi sig wallet
#[derive(Clone, Eq, PartialEq, Encode, Decode, Default, RuntimeDebug)]
pub struct MultiSigConfig<AccountId, Hash> {
	/// the members that co-manage a multi sig wallet
	members: Vec<AccountId>,
	/// declare the threshold that once it is reached,
	/// the proposal will be executed by the multi sig wallet 
	threshold: Percent,
	/// the hash of proposals that not been executed, waiting for 
	current_proposals: Vec<Hash>
}

impl<AccountId: PartialEq + Ord, Hash: PartialEq> MultiSigConfig<AccountId, Hash> {

	pub fn new(members: Vec<AccountId>, threshold: Percent) -> Self {
		MultiSigConfig {
			members, threshold, current_proposals: vec![]
		}
	}

	pub fn new_proposal(&mut self, new_proposal: Hash) {
		 if !self.current_proposals.contains(&new_proposal) {
			 self.current_proposals.push(new_proposal)
		 }
	}

	pub fn remove_proposal(&mut self, proposal:&Hash) {
		self.current_proposals.retain(|p| p != proposal);
	}

	pub fn add_member(&mut self, who: AccountId) {
		if !self.members.contains(&who) {
			self.members.push(who);
		}
	}

	pub fn remove_member(&mut self, who: &AccountId) {
		if self.members.contains(who) {
			self.members.retain(|m| m != who)
		}
	}

	pub fn is_member(&self, who: &AccountId) -> bool {
		self.members.binary_search(who).is_ok()
	}
}

/// something that can prepresent the current status of a proposal
#[derive(Clone, Eq, PartialEq, Encode, Decode, Default, RuntimeDebug)]
pub struct ProposalStatus<AccountId> {
	/// if this proposal is approved, the account taht should execute it
	multisig_wallet: AccountId,
	/// the members who have approved this proposal
	approved_members: Vec<AccountId>,
	/// ready to execute or not
	ready_to_execute: bool,
}


impl<AccountId: PartialEq + Ord> ProposalStatus<AccountId> {
	fn new(multisig_wallet: AccountId, initiator: AccountId) -> Self {
		ProposalStatus {
			multisig_wallet,
			approved_members: vec![initiator],
			ready_to_execute: false
		}
	}

	fn update_execution_status(&mut self, ready_to_execute: bool) {
		self.ready_to_execute = ready_to_execute
	}

	fn update_approved_members(&mut self, new: Vec<AccountId>) {
		self.approved_members = new;
	}

	fn is_right_multisig(&self, address: &AccountId) -> bool {
		&self.multisig_wallet == address
	}

	fn is_ready_to_execute(&self) -> bool {
		self.ready_to_execute
	}

	fn has_already_approved(&self, who: &AccountId) -> bool {
		self.approved_members.contains(who)
	}
}


pub struct MultiSigAddressGenerator<T: Trait>(PhantomData<T>);
impl<T: Trait> AddressDeterminator<T::AccountId, T::BlockNumber, T::Index> for MultiSigAddressGenerator<T>
where
	T::AccountId: UncheckedFrom<T::Hash> + AsRef<[u8]> {
	fn multi_sig_address(creator: &T::AccountId, block_number: T::BlockNumber, salt: T::Index) -> T::AccountId {
		let mut buf = Vec::new();
		buf.extend_from_slice(&block_number.saturated_into::<u64>().to_be_bytes()[..]);
		buf.extend_from_slice(&salt.saturated_into::<u64>().to_be_bytes()[..]);
		buf.extend_from_slice(creator.as_ref());
		
		UncheckedFrom::unchecked_from(T::Hashing::hash(&buf[..]))
	}
}

/// The pallet's configuration trait.
pub trait Trait: system::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	/// an general call
	type Proposal: Parameter + Dispatchable<Origin=Self::Origin> + GetDispatchInfo;
	/// to generate random address for multi sig wallet
	type AddrGenerator: AddressDeterminator<Self::AccountId, Self::BlockNumber, Self::Index>;
}


// This pallet's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as MultiSig {
		/// each multi-sig wallet's configuration
		/// the key is the multi sig wallet's `AccountId`
		pub MultiSig get(fn multi_sig): map hasher(blake2_256) T::AccountId => Option<MultiSigConfig<T::AccountId, T::Hash>>;
		/// here to store each proposal's current status, if a poposal
		/// has been removed or executed we just remove its status
		/// the key is the hash of each proposal
		pub Proposals get(fn proposals): map hasher(blake2_256) T::Hash => Option<ProposalStatus<T::AccountId>>;
	}
}

// The pallet's events
decl_event!(
	pub enum Event<T> where 
		AccountId = <T as system::Trait>::AccountId,
		Hash = <T as system::Trait>::Hash {
		/// a multisig wallet has been created
		MultiSigCreated(AccountId),
		/// submit a propose 
		ProposeSubmitted(Hash, AccountId),
		/// propose executed
		Executed(Hash),
	}
);

// The pallet's errors
decl_error! {
	pub enum Error for Module<T: Trait> {
		/// at least 2 accounts in the membership
		/// when in the process of execution, it means
		/// the proposal is not ready to execute
		NotEnoughMembers,
		/// make sure the creator of a multi sig wallet 
		/// in the member list
		MissedMember,
		/// threshold should not be zero
		ZeroThreshold,
		/// the pre-calculated multi sig wallet
		/// has been already existed
		MultiSigExisted,
		/// MultiSig address should exist when
		/// the member propose
		UnknownAddress,
		/// there are duplicated account in the members
		/// reserved for future use
		DuplicatedMembers,
		/// 1. the sender has no rights to propose 
		/// onbehalf of some specific multisig wallet
		NotAllowed,
		/// Wrong order passing into runtime mod
		WrongOrder,
		/// Double-approve error
		/// a member is not allowed to propose more than 
		/// once for the same proposal
		DoubleApprove,
		/// Not a member of specific multisig wallet
		NotAMember,
		/// Proposal does not exist
		ProposalNotExists,

	}
}

// The pallet's dispatchable functions.
decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		/// expose the multisig error to metadata
		type Error = Error<T>;

		// Initializing events
		fn deposit_event() = default;

		/// create a new multi sig wallet
		#[weight = SimpleDispatchInfo::FixedNormal(100_000)]
		fn create_multisig_wallet(origin, members: Vec<T::AccountId>, threshold: Percent) -> DispatchResult {
			let who = ensure_signed(origin)?;
			// pre generate a new multisig wallet address
			let multi_sig_addr = T::AddrGenerator::multi_sig_address(
				&who, 
				<system::Module<T>>::block_number(), 
				<system::Module<T>>::account_nonce(&who));
			// ensure the pre-generated address does not exist
			ensure!(Self::multi_sig(&multi_sig_addr).is_none(), Error::<T>::MultiSigExisted);
			ensure!(!threshold.is_zero(), Error::<T>::ZeroThreshold);
			let mut members = members;
			ensure!(members.contains(&who), Error::<T>::MissedMember);
			
			// remove duplicated accounts in the member list
			// make sure that each member is unique
			// RECOMMEND to do this offchain to not waste resources onchain 
			ensure!(Self::is_sort_and_unique(&members), Error::<T>::WrongOrder);
			members.dedup();
			ensure!(members.len() > 1, Error::<T>::NotEnoughMembers);
			// create a new multi sig
			let multisig_config = MultiSigConfig::new(members, threshold);
			<MultiSig<T>>::insert(&multi_sig_addr, multisig_config);
			Self::deposit_event(RawEvent::MultiSigCreated(multi_sig_addr));
			Ok(())
		}

		/// propose a new or existing proposal
		#[weight = SimpleDispatchInfo::FixedNormal(100_000)]
		fn propose(origin, multisig: T::AccountId, call: Box<<T as Trait>::Proposal>) -> DispatchResult {
			let who = ensure_signed(origin)?;
			// to ensure that this multisig wallet address exists
			let config = Self::multi_sig(&multisig).ok_or(Error::<T>::UnknownAddress)?;
			ensure!(config.is_member(&who), Error::<T>::NotAllowed);
			// compute the hash of the proposal
			let call_hash = T::Hashing::hash(&call.clone().encode()[..]);
			if let Some(proposal_status) = Self::proposals(&call_hash) {
				// process exsiting proposal
				let mut proposal_status = Self::proposals(&call_hash).unwrap();
				ensure!(proposal_status.is_right_multisig(&multisig), Error::<T>::NotAllowed);
				ensure!(!proposal_status.has_already_approved(&who), Error::<T>::DoubleApprove);
				let ready_to_execute = Self::process_proposal(&multisig, who.clone(), &mut proposal_status);
				proposal_status.update_execution_status(ready_to_execute);
				<Proposals<T>>::insert(&call_hash, proposal_status);
				
			} else {
				// initialize
				let proposal_status = ProposalStatus::new(multisig.clone(), who.clone());
				<Proposals<T>>::insert(&call_hash, proposal_status);
				// add to the multisig config
				// TODO: check the mutability if the value is Option<T>
				<MultiSig<T>>::mutate(&multisig, |o|{
					if let Some(config) = o {
						config.new_proposal(call_hash);
					};
				});
			}
			Self::deposit_event(RawEvent::ProposeSubmitted(call_hash, who));
			Ok(())
		}

		#[weight = SimpleDispatchInfo::FixedNormal(100_000)]
		fn execute_proposal(origin, multisig: T::AccountId, call: Box<<T as Trait>::Proposal>) -> DispatchResult {
			let who = ensure_signed(origin)?;
			let mut config = Self::multi_sig(&multisig).ok_or(Error::<T>::UnknownAddress)?;
			let call_hash = T::Hashing::hash(&call.clone().encode()[..]);
			let proposal = Self::proposals(&call_hash).ok_or(Error::<T>::ProposalNotExists)?;
			ensure!(proposal.is_ready_to_execute(), Error::<T>::NotEnoughMembers);
		
			// remove before execution, whether the result is a success or failure
			Self::internal_remove_proposal(&call_hash, &mut config);
			// update the current proposals in storage
			<MultiSig<T>>::insert(&multisig, config);
			Self::deposit_event(RawEvent::Executed(call_hash));
			return call.dispatch(system::RawOrigin::Signed(multisig).into());
		}

		/// Can only be called by multisig wallet
		/// to remove a given member
		#[weight = SimpleDispatchInfo::FixedNormal(100_000)]
		fn remove_member(origin, member: T::AccountId) -> DispatchResult {
			let who = ensure_signed(origin)?;
			if let Some(config) = Self::multi_sig(&who) {
				ensure!(config.members.contains(&member), Error::<T>::DuplicatedMembers);
				<MultiSig<T>>::mutate(&who, |o| {
					if let Some(config) = o {
						config.remove_member(&member);
					}
				}); 
				Ok(())
			} else {
				Err(Error::<T>::UnknownAddress)?
			}
		}

		/// Can only be called by multisig wallet
		/// add a new member to the member list
		#[weight = SimpleDispatchInfo::FixedNormal(100_000)]
		fn add_member(origin, new_member: T::AccountId) -> DispatchResult {
			let who = ensure_signed(origin)?;
			if let Some(config) = Self::multi_sig(&who) {
				ensure!(!config.members.contains(&new_member), Error::<T>::NotAMember);
				<MultiSig<T>>::mutate(&who, |o| {
					if let Some(config) = o {
						config.add_member(new_member);
					}
				}); 
				Ok(())
			} else {
				Err(Error::<T>::UnknownAddress)?
			}
		}

		// TODO: ready for hacking
		// [ ] update threshold 
		// [ ] introduce weights to gain finer-grained measurement for each member
	}
}

impl<T: Trait> Module<T> {

	fn is_sort_and_unique(members: &Vec<T::AccountId>) -> bool {
		members.windows(2).all(|m| m[0] < m[1])
	}

	/// do some neccesary check before the execution
	/// return false to denote that it's not ready for the execution
	/// true to trigger the execution
	fn process_proposal(
		multisig_address: &T::AccountId,
		proposer: T::AccountId, 
		proposal_status: &mut ProposalStatus<T::AccountId>
	) -> bool
	{
		// add the latest proposer to the approved list
		let mut approved_members = proposal_status.approved_members.clone();
		approved_members.push(proposer);
		// get the multisig config for further computation
		let multisig_config = Self::multi_sig(multisig_address).unwrap();
		let members = multisig_config.members;
		let total = members.len() as u8;
		// remove invalid members from the approved list
		let valid_approved_members = Self::trim_invalid(members, approved_members);
		let part = valid_approved_members.len() as u8;
		// update the valid list in current proposal status
		proposal_status.update_approved_members(valid_approved_members);

		// to check that if the proposal is ok to execute
		if !Self::is_over_threshold(part, total, multisig_config.threshold) {
			false
		} else {
			true
		}

	}

	/// members of a multisig wallet can be removed at anytime
	/// make sure there are no outdated members in any unexecuted proposal
	fn trim_invalid(total_members: Vec<T::AccountId>, approved_members: Vec<T::AccountId>) -> Vec<T::AccountId> {
		let total_members_set: BTreeSet<T::AccountId> = total_members.into_iter().collect();
		let approved_members_set: BTreeSet<T::AccountId> = approved_members.into_iter().collect();
		// find all the members in approved list that are not among the membership
		// if all approved members are valid then the result is empty
		approved_members_set.intersection(&total_members_set).cloned().collect()
		// let difference = approved_members_set.difference(&total_members_set).cloned().collect::<BTreeSet<_>>();
		// (&approved_members_set - &difference).into_iter().collect::<Vec<T::AccountId>>()
	}

	/// approximatelly
	fn is_over_threshold(part: u8, total: u8, threshold: Percent) -> bool {
		Percent::from_rational_approximation(part, total) >= threshold
	}

	fn internal_remove_proposal(
		call_hash: &T::Hash, 
		config: &mut MultiSigConfig<T::AccountId, T::Hash>) 
	{
		// delete status
		<Proposals<T>>::remove(call_hash);
		// remove from the multisig's waiting queue
		config.remove_proposal(call_hash);
	}
}