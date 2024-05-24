local git_deployment = confer.fact({
  name = "git",
  source = "./git",
  destination = "~/"
})

local zsh_deployment = confer.fact({
  name = "zsh",
  sources = {".zsh/", ".zshrc"},
  destination = "~/"
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "./kitty",
  destination = "~/.config/"
})

local laptop = confer.deploy({
  hostname = "my-laptop",
  facts = {
    git_deployment,
    zsh_deployment,
    kitty_deployment,
    },
})

local server = confer.deploy({
  hostname = "my-server",
  facts = {
    git_deployment,
    zsh_deployment,
    },
})

return {
  laptop,
  server
}
