use super::*;

use frame_system::RawOrigin;
use frame_benchmarking::{benchmarks, account};
use sp_runtime::traits::{Bounded, Dispatchable};

const SEED: u32 = 0;

benchmarks! {
    _ {
        let u in 1..1000 => ();
    }

    create_multisig_wallet {
        let u in ...;

        let creator: T::AccountId = account("creator", u, SEED);
        let member1 = account("member1", u, SEED);
        let member2 = account("member2", u, SEED);
        let mut members = vec![creator.clone(), member1, member2];
        members.sort();

    }: _(RawOrigin::Signed(creator), members, Percent::from_percent(50))
}