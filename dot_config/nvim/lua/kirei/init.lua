--[[
Flat require-all logic. This code automatically `require`s all files in this
dir.

Useful for an init.lua file, so you don't have to manually require any new
modules you add in the dir.

Don't forget to add this file name to the `ignore` table!
--]]

local my_info = debug.getinfo(1, 'S').source
local my_dir = my_info:match('^@(.*)/')

local files = vim.loop.fs_scandir(my_dir)
if not files then
  error(('Error requiring modules in %s'):format(my_dir))
end

local ignore = {
  constants = true,
  init = true, --self
  utils = true,
}

while files do
  local file, ftype = vim.loop.fs_scandir_next(files)

  if not file then break end

  if ftype ~= 'file' then goto continue end

  local name = vim.fn.fnamemodify(file, ':r')
  if ignore[name] then goto continue end

  local ext = vim.fn.fnamemodify(file, ':e')
  if ext ~= 'lua' then goto continue end

  require(... .. '.' .. name)

  ::continue::
end
