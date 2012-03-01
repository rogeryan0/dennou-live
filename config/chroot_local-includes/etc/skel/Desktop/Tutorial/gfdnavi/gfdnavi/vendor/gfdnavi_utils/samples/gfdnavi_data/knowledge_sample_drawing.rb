user = User.find(:first, :conditions => ["login=?", "root"])
gl_k = GfdnaviLocal.parse_path("/samples/reanalysis/ncep/T.jan.nc/T")
image = gl_k.plot("tone")[0]
image.save_as("_test_image01.png", user)
