local function init()
    require 'fevy.vim'.init()
    require 'fevy.theme'.init()
    require 'fevy.languages'.init()
    require 'fevy.floaterm'.init()
    require 'fevy.telescope'.init()
end

return {
    init = init,
}