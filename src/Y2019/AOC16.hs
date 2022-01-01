module Y2019.AOC16
    ( solution1
    , solution2
    )
where

solution1 :: IO String
solution1 = return . format $ phases 100 input

solution2 :: IO String
solution2 = return . format . offset $ phases 100 (mconcat $ replicate 10000 input)

basePattern :: [Int]
basePattern = [0, 1, 0 , -1]

phasePatternN :: Int -> [Int]
phasePatternN n = tail . cycle . mconcat $ replicate n <$> basePattern

nth :: Int -> [a] -> a
nth 0 as     = head as
nth x (_:as) = nth (x-1) as
nth _ _      = error "invalid index access"

phase :: [Int] -> [Int]
phase xs = let ph p = flip mod 10 . abs . sum $ zipWith (\n s -> s * nth n (phasePatternN p)) [0..] xs in
    zipWith (\n _ -> ph n) [1..] xs

phases :: Int -> [Int] -> [Int]
phases 0 xs = xs
phases n xs = phases (n-1) $ phase xs

charToNum :: Enum a => a -> Int
charToNum a = fromEnum a - fromEnum '1' + 1

_example :: [Int]
_example = charToNum <$> "12345678"

_longExample :: [Int]
_longExample = charToNum <$> "80871224585914546619083218645595"

format :: [Int] -> [Char]
format xs = toEnum . pred . (+ fromEnum '1') <$> take 8 xs

offset :: [a] -> [a]
offset = drop 5976809

input :: [Int]
input = charToNum <$> "59768092839927758565191298625215106371890118051426250855924764194411528004718709886402903435569627982485301921649240820059827161024631612290005106304724846680415690183371469037418126383450370741078684974598662642956794012825271487329243583117537873565332166744128845006806878717955946534158837370451935919790469815143341599820016469368684893122766857261426799636559525003877090579845725676481276977781270627558901433501565337409716858949203430181103278194428546385063911239478804717744977998841434061688000383456176494210691861957243370245170223862304663932874454624234226361642678259020094801774825694423060700312504286475305674864442250709029812379"
