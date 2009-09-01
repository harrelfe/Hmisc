if(.R.) {
  if(packageDescription('survival',fields='Version') >= "2.35-3")
    survfitKM <- survival:::survfitKM
}
