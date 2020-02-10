// Creating mock runtime here
use super::*;
use crate as multisig;
use sp_core::H256;
use frame_support::{impl_outer_origin, impl_outer_event, impl_outer_dispatch, parameter_types, weights::Weight};
use sp_runtime::{
	traits::{BlakeTwo256, IdentityLookup}, testing::Header, Perbill,
};
use frame_system::{ self as system };


impl_outer_origin! {
	pub enum Origin for Test where system = frame_system {}
}

impl_outer_event! {
	pub enum TestEvent for Test {
		multisig<T>,
	}
}
impl_outer_dispatch! {
	pub enum Call for Test where origin: Origin {
		multisig::Multisig,
	}
}


// For testing the module, we construct most of a mock runtime. This means
// first constructing a configuration type (`Test`) which `impl`s each of the
// configuration traits of modules we want to use.
#[derive(Clone, Eq, PartialEq)]
pub struct Test;
parameter_types! {
	pub const BlockHashCount: u64 = 250;
	pub const MaximumBlockWeight: Weight = 1024;
	pub const MaximumBlockLength: u32 = 2 * 1024;
	pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
}

impl frame_system::Trait for Test {
	type Origin = Origin;
	type Index = u64;
	type BlockNumber = u64;
	type Hash = H256;
	type Call = Call;
	type Hashing = BlakeTwo256;
	type AccountId = u64;
	type Lookup = IdentityLookup<Self::AccountId>;
	type Header = Header;
	type Event = TestEvent;
	type BlockHashCount = BlockHashCount;
	type MaximumBlockWeight = MaximumBlockWeight;
	type MaximumBlockLength = MaximumBlockLength;
	type AvailableBlockRatio = AvailableBlockRatio;
	type Version = ();
	type ModuleToIndex = ();
}
// mocked address generator
pub struct DummyAddressGenerator;
impl AddressDeterminator<u64, u64, u64> for DummyAddressGenerator {
	fn multi_sig_address(creator: &u64, block_number: u64, salt: u64) -> u64 {
		creator.to_owned() + block_number + salt
	}
}

impl Trait for Test {
	type Event = TestEvent;
	type Proposal = Call;
	type AddrGenerator = DummyAddressGenerator;
}

pub type System = system::Module<Test>;
pub type Multisig = Module<Test>;

pub type MultiSigCall = super::Call<Test>;

pub fn call_hash(call: Box<Call>) -> H256 {
	BlakeTwo256::hash(&call.clone().encode()[..])
}

// This function basically just builds a genesis storage key/value store according to
// our desired mockup.
pub fn new_test_ext() -> sp_io::TestExternalities {
	system::GenesisConfig::default().build_storage::<Test>().unwrap().into()
}
