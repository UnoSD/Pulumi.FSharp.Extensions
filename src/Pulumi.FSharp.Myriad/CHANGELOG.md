# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.2.0] - 2024-10-01

Support for `ResourceOptions`-derived types has been improved, allowing more flexible customization 

### Added
- Various custom operations that represent `ResourceOptions` in the Pulumi .NET SDK.

### Fixed
- Fixed an issue where specific resource types could not be used as inputs to other Pulumi resources (such as in the AWS providers >6.49)
- Some providers such as AWS and Docker, which use inconsistent casing for resource names, now handle names case-insensitivitely.

## [3.1.7] - 2024-06-24

Pulumi providers that use `ComponentResource` as a base class are now interpreted correctly by the Myriad compiler extension.

## [3.1.6] - 2024-05-31

Providers now use a dedicated FAKE dependency group to enable detection of updated providers via GitOps

### Changed
- Provider dependencies moved from `Main` group to `Providers`.

## [3.1.5] - 2024-05-31

Updated the project to use modern build tooling.

### Fixed
- Numbers in provider names (e.g. `auth0`) no longer break the plugin.
- GCP resources that use slashes as namespacing (e.g. `gcp:privilegedaccessmanager/entitlement`) no longer break the plugin.

### Added
- DigitalOcean provider
- Auth0 provider
- Support for Kubernetes `ConfigFile` and `ConfigGroup` types