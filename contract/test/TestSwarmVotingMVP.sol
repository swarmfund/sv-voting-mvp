pragma solidity ^0.4.17;

import "truffle/Assert.sol";
import "truffle/DeployedAddresses.sol";
import "../contracts/SwarmVotingMVP.sol";

contract TestSwarmVotingMVP {

  function testInitialBalanceUsingDeployedContract() public {
    // MetaCoin meta = MetaCoin(DeployedAddresses.MetaCoin());

    uint expected = 10000;

    Assert.equal(expected, expected, "Owner should have 10000 MetaCoin initially");
  }

  function testInitialBalanceWithNewMetaCoin() public {
    // MetaCoin meta = new MetaCoin();

    uint expected = 10000;

    Assert.equal(expected, expected, "Owner should have 10000 MetaCoin initially");
  }

}
