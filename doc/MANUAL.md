## NAME

confer – The dotfiles manager

## SYNOPSIS

<dl>
    <dt>confer deploy <i>[OPTION]</i></dt>
    <dd style="margin-left: 1rem;">Deploy your configuration files as symbolic links in their configured destination</dd>
    <dt>confer check <i>[OPTION]</i></dt>
    <dd style="margin-left: 1rem;">Ensure that the configured link destinations do not exist.</dd>
</dl>

## DESCRIPTION

*confer* is a dotfiles manager that handles the deployment and synchronisation of your configuration files.

It is configured in Lua. The [Lua API](#lua-api) enables you to decide where to link your files depending on criteria, like your operating system.

## OPTIONS

### Runtime Options

#### `--dry-run`
Simulate the actions to be executed but doesn't do them.

#### `--deployments-file`
Use the specified deployments.lua file

#### `--verbose`
Make the execution more verbose about what it does.

#### `--version`
Display the version of the tool.

#### `--help`
Display the help message.

### Configuration overrides

#### `--arch=<arch>`
Override the detected architecturein the configuration file.

With `arch` as:

* aarch64
* x86_64

#### `--os=<os>`
Override the operating system detected in the configuration file.

With `os` as:
* darwin
* freebsd
* linux
* windows

#### `--hostname=<hostname>`
Override the host name detected in the configuration file.

### CONFIGURATION

The configuration is written in a Lua file with facts and deployement rules.

To express a symbolic link of your `.gitconfig` file within your home directory, write:

```lua
local git_deployment = confer.fact({
  -- The name of this fact.
  name = "git",
  -- The file or directory that you want to link.
  source = ".gitconfig",
  -- The directory in which the link will be made.
  destination = user.home .. "/.gitconfig"
})
```
Then we add a rule that holds potential conditions for this deployment to occur:
The name of the host has to be `my-laptop`. If this condition is not met,
the deployment will be ignored.

```lua
local laptop = confer.deploy({
  hostname = "my-laptop",
  facts = {
    git_deployment,
    },
})

```

### EXAMPLES

#### Example configuration

```lua
local user = require("user")

-- Facts

local git_deployment = confer.fact({
  name = "git",
  source = ".gitconfig",
  destination = user.home .. "/.gitconfig"
})

local zsh_deployment = confer.fact({
  name = "zsh",
  source = ".zsh",
  destination = user.home .. "/.zsh"
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "kitty",
  destination = user.home .. "/.config/kitty"
})

local irssi_deployment = confer.fact({
  name = "irssi",
  source = ".irssi",
  destination = user.home .. "/.irssi"
})

-- Deployments

local allMachines = confer.deploy({
  facts = {
    git_deployment,
    zsh_deployment,
  }
})

local laptop = confer.deploy({
  hostname = "my-laptop",
  facts = {
    kitty_deployment,
    },
})

local server = confer.deploy({
  hostname = "my-server",
  facts = {
    irssi_deployment
    },
})

return {
  allMachines,
  laptop,
  server
}
```

## Lua API

Confer provides its own utilities to be used in Lua:

### Host API

#### `host.os`:
The operating system identifier of the host.
It is either inferred from your host, or overriden by the `--os` command-line option.

Possible values:
* darwin
* freebsd
* linux
* windows

#### `host.arch`:
The architecture identifier of the host.
It is either inferred from your host, or overriden by the `--arch` command-line option.

Possible values:
* aarch64
* x86_64

#### `host.hostname`
The hostname of the host.
It is either inferred from your host, or overriden by the `--hostname` command-line option.

### User API

#### `user.home`
The home directory of the current user.

### Confer API

<dl>
  <dt>FilePath</dt>
  <dd style="margin-left: 1rem;">OS-specific string that represents the location of file or directory</dd>

  <dt>String</dt>
  <dd style="margin-left: 1rem;">UTF-8 encoded sequence of characters</dd>
</dl>

#### `confer.fact`
This function takes a dictionary (of type `Fact`) which takes the following constructors:
* name: String, name of this deployment
* source: FilePath, directory or file which is going to be linked to the destination
* destination: FilePath, absolute path to which the `source` will be linked

#### `confer.deploy`
* facts: Array Fact, all the facts you want to run
* hostname: String, the hostname of the machine in which the deployment will occur. If not specified, will occur on all hosts.
* architecture: String, the architecture of the machine in which the deployment will occur. If not specified, will occur on all architectures.
* os: String, the operating system of the machine in which the deployment will occur. If not specified, will occur on all operating systems.
