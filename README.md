# A Repository where I do explorations on random topics

This was created to avoid multiple different repositories. It is intended to be a centralized place where I can host my interests and different programming topics. It is inspired by Daniel Shiffman's coding challenges

Each project is in a subdirectory and contains a README explaining the motivations, research done and how to execute the code in the project
Each sufficiently large project may be extracted into its individual repo

## Building and Technologies 

Requirements: nix
A flake.nix will be at every project root and to create an development shell simply run `nix develop`

Each project may use varying technologies and the individual requirements and build instructions will be detailed in the relevant README. The common requirement however is nix which should allow a consistent environment and a centralized place for the dependencies
