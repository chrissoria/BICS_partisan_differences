-- wordcount.lua
-- Counts words in the document body, excluding figure captions.
local words = 0

local function count(el)
  if el.text and el.text:match("%P") then
    words = words + 1
  end
end

local wordcount_walker = {
  Str  = count,
  Code = function(el)
    for _ in el.text:gmatch("%S+") do words = words + 1 end
  end,
}

function Pandoc(el)
  -- walk top-level blocks, skipping Figure blocks so captions don't count
  for _, block in ipairs(el.blocks) do
    if block.t ~= "Figure" then
      pandoc.walk_block(block, wordcount_walker)
    end
  end
  -- append word count paragraph at the end
  local note = pandoc.Para({
    pandoc.Strong({pandoc.Str("Word count (excl. figure caption): " .. words)})
  })
  table.insert(el.blocks, note)
  return el
end
