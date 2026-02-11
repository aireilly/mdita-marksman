---
author: Aidan Reilly
keyword: [configuration, settings, toml]
---

# Configuration

MDITA Marksman supports user-level and project-level configuration for customizing its behavior.

1. User-level configuration is read from `$HOME/.config/mdita-marksman/config.toml`.
2. Project-level configuration is read from `.mdita-marksman.toml` located in the project's root folder.

For each configuration option the precedence is: project config > user config > global default.

[This config file](../Tests/default.mdita-marksman.toml) shows all configuration options with their
default values. You need to specify ONLY the options you wish to override in your user- or
project-config.
