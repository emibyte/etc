local function init()
    require 'emibyte.vim'.init()
    require 'emibyte.theme'.init()
    require 'emibyte.languages'.init()
    require 'emibyte.floaterm'.init()
    require 'emibyte.telescope'.init()
    require 'emibyte.cmp'.init()
end

return {
    init = init,
}
