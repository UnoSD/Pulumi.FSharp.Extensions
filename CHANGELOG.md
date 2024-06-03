# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.1.5] - 2024-05-31

Updated the project to use modern build tooling.

### Fixed
- Numbers in provider names (e.g. `auth0`) no longer break the plugin.
- GCP resources that use slashes as namespacing (e.g. `gcp:privilegedaccessmanager/entitlement`) no longer break the plugin.

### Added
- DigitalOcean provider
- Auth0 provider
- Support for Kubernetes `ConfigFile` and `ConfigGroup` types