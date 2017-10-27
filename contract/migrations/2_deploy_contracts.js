var SwarmVotingMVP = artifacts.require("./SwarmVotingMVP.sol");

module.exports = function(deployer) {
  var startTime = Math.round(Date.now() / 1000) + 1;
  var endTime = startTime + 600;
  deployer.deploy(SwarmVotingMVP, startTime, endTime, "0xba781ed1006bd7694282a210485265f1c503f4e6721858b4269ae6d745f7bb4b", true);
};

