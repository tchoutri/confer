local user = require("user")

-- Facts

local git_deployment = confer.fact({
  name = "git",
  source = ".gitconfig",
  destination = user.home
})

local zsh_deployment = confer.fact({
  name = "zsh",
  source = ".zsh",
  destination = user.home
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "kitty",
  destination = user.home .. "/.config"
})

local irssi_deployment = confer.fact({
  name = "irssi",
  source = ".irssi",
  destination = user.home
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
