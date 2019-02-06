<h1 align="center">
  <br/>
  <a href='https://github.com/ConsenSys/web3studio-bootleg'><img
      width='250px'
      alt='Bootleg'
      src="https://user-images.githubusercontent.com/5770007/52348724-02aa0780-29f3-11e9-9039-71880d1af2b6.png" /></a>
  <br/>
</h1>

<h4 align="center">
  Steal this Code
</h4>

<p align="center">
  <a href="#packages">Packages</a> ∙
  <a href="#license">License</a>
</p>

Welcome to the near future. This repo is the home of **Bootleg** 👢🦵. This is
the latest project from the [Consensys Web3Studio](https://consensys.net/web3studio/) team.
We are beginning development on this exciting new idea and you can follow the
latest updates here through GitHub. Please join the discussion, create issues, and get involved!

Read more about the project in the latest article about Bootleg on [Medium](https://link.medium.com/ZgnzmtPMrT)

<br/>

## Packages

This is a monorepo that contains the projects that make bootleg rock 🎸. You can find
the code for all of them in in the [`packages`](packages) folder.

You'll notice two prefixes. `bootleg` projects are directly related to the fictional
bootleg app, or the smart contracts it uses. `helix` projects are related to a system to
create non-transferable digital native assets, basically blockchain DRM.

### [`bootleg-tokens`](packages/bootleg-tokens)

This is where you can find the smart contracts for the bootleg token itself. This is where you
would want to look if you want to use our pay-it-backwards tokens in your dApp.

### [`bootleg-app-contracts`](packages/bootleg-app-contracts)

Here lies the smart contracts used by our fictional app. It's a great reference
implementation for your Bootleg-esk dApp.

### [`helix-crypto`](packages/helix-crypto)

Where the rubber hits the road. A phenomenal library to encrypt and then cryptographically
watermark an asset to allow copyright holders to find who leaked their content.

### [`helix-contracts`](packages/helix-contracts)

Reference contracts that the Helix system uses to make everything trustless.

### [`examples`](packages/examples)

Everything comes together here. This is a simulated environment where you can see how to wire
everything up and even tweak with some parameters for your dApp.

## License

As per usual, we are publishing under the [Apache 2.0 License](LICENSE).
