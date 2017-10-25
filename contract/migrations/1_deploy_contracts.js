var SwarmVotingMVP = artifacts.require("./SwarmVotingMVP.sol");

module.exports = function(deployer) {
  deployer.deploy(SwarmVotingMVP, 0, 1600000000, "0x85cc9e15d80493d5b0f62e21fe54703bf04471de715b63fcfe33a95e7cc83b2d");
};

