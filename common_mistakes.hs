-- -- Guards
-- -- Dont use an if inside a Guard
-- myfunction x y
--     | if x < 2           = "a"
--     | if y > 20          = "b"
--     | otherwise          = "c"

-- ---------------------------------------- --
-- -- Indentation
-- -- wrong
-- f x y = do  if x < y then
--         x
--     else
--         y

-- -- correct
-- f x y = do  if x < y
--                 then x
--                 else y

-- ---------------------------------------- --
-- -- If / Else
-- If always needs an else!

f = let 
        x = 0
        y = 1
    in x + y