# CI-Server

This is the repository of Group 9 for Assignment #2: Continuous Integration in DD2480.

![unit test](https://github.com/TerenceNg03/CI-Server/actions/workflows/test.yml/badge.svg)
![document](https://github.com/TerenceNg03/CI-Server/actions/workflows/docs.yml/badge.svg)
![coding style](https://github.com/TerenceNg03/CI-Server/actions/workflows/style.yml/badge.svg)

![Codecov](https://img.shields.io/codecov/c/github/TerenceNg03/CI-Server)
![GitHub issues](https://img.shields.io/github/issues/TerenceNg03/CI-Server)
![GitHub pull requests](https://img.shields.io/github/issues-pr/TerenceNg03/CI-Server)

## Usage

### Dependencies

Our server is written in Haskell and is managed using Stack, which can be installed [here](https://docs.haskellstack.org/en/stable/). We use [Maven](https://maven.apache.org/) as the package manager for our project, which can be installed with the following commands:

```sh
$ sudo apt update
$ sudo apt install maven
```

### Running the Server

The server needs a config file to run (an example config file can be found at `/server/config-example.yaml`). With Stack installed and your working directory set to `/server`, the following commands can be run:

```sh
$ stack test # Run tests
$ stack build # Build the Stack project
$ stack run  # Run the web server (default config file is "./config.yaml")
$ stack run -- <Path to your config file> # Run the web server
```

With Maven installed, the following commands can be run from the root directory:

```sh
$ mvn package # Build the project and run unit tests
$ mvn test # Only run the tests without rebuild
$ mvn exec:java # Run the main function
$ mvn site # Generate documentation
```

### Further Dependencies

- Maven 3.6+ requires JDK 8 or above to execute.
- Additional dependencies for Maven are configured in `pom.xml`, while those for Stack are specified in `/server/package.yaml` and `/server/stack.yaml`.

## Coding Standard

We follow [Google's Java Style](https://github.com/diffplug/spotless/tree/main/plugin-maven#google-java-format) and enforce the standard via [Spotless](https://github.com/diffplug/spotless). For Haskell, we follow [Fourmolu's Haskell Style](https://fourmolu.github.io/) and enforce the standard via [Fourmolu](https://hackage.haskell.org/package/fourmolu).

## API Documentation

API Documentation is automatically generated by [JavaDoc](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javadoc.html) and [Haddock](https://haskell-haddock.readthedocs.io/en/latest/,) and is deployed on [Github Pages](https://terenceng03.github.io/CI-Server/). Haskell documentation can be found [here](https://terenceng03.github.io/CI-Server/doc).

## Team

### Seeded

- [x] The team mission has been defined in terms of the opportunities and outcomes.
- [x] Constraints on the team's operation are known.
- [ ] Mechanisms to grow the team are in place.
- [x] The composition of the team is defined.
- [x] Any constraints on where and how the work is carried out are defined.
- [x] The team's responsibilities are outlined.
- [x] The level of team commitment is clear.
- [x] Required competencies are identified.
- [x] The team size is determined.
- [x] Governance rules are defined.
- [x] Leadership model is determined.

### Formed

- [x] Individual responsibilities are understood.
- [x] Enough team members have been recruited to enable the work to progress.
- [x] Every team member understands how the team is organized and what their individual role is.
- [x] All team members understand how to perform their work.
- [x] The team members have met (perhaps virtually) and are beginning to get to know each other.
- [x] The team members understand their responsibilities and how they align with their competencies.
- [x] Team members are accepting work.
- [x] Any external collaborators (organizations, teams and individuals) are identified.
- [x] Team communication mechanisms have been defined.
- [x] Each team member commits to working on the team as defined.

### Collaborating

- [x] The team is working as one cohesive unit.
- [x] Communication within the team is open and honest.
- [x] The team is focused on achieving the team mission.
- [x] The team members know and trust each other.

### Performing

- [x] The team consistently meets its commitments.
- [x] The team continuously adapts to the changing context.
- [x] The team identifies and addresses problems without outside help.
- [x] Effective progress is being achieved with minimal avoidable backtracking and reworking.
- [x] Wasted work and the potential for wasted work are continuously identified and eliminated.

### Adjourned

- [x] The team responsibilities have been handed over or fulfilled.
- [x] The team members are available for assignment to other teams.
- [x] No further effort is being put in by the team to complete the mission.

## Contributions

Contributions to the projected is stated below.

### Angelica Engström

- Implement CI Feature for Automatic Testing
- Implement Output of CI Features

### Celina Tjärnström

- Implement Functionality for Compiling and Testing Branch

### Tobias Hansson

- Implement Git Cloning of Related Repository

### John Söderholm

- Implement Build History Database
- Implement SQLite Queries to Save and Read Build History

### Tianxing Wu

- Implement Web Request Handling
- Implement POST Commit Status
- Set Up Project
