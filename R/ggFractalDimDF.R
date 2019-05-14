ggFractalDimDF =function(DimData){
  list(
    g_S_n = ggplot(data =DimData,aes(x = S,y = n)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue'),
    g_S_1_n = ggplot(data =DimData,aes(x = S_1,y = n)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue'),
    g_logS_logn_1 = ggplot(data =DimData,aes(x = logS,y = logn_1)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue'),
    g_logn_logS_1 = ggplot(data =DimData,aes(x = logn,y = logS_1)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue'),
    g_S_d=ggplot(data =DimData,aes(x = S,y = d)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue'),
    g_S_1_d=ggplot(data =DimData,aes(x = S_1,y = d)) +
      geom_point() +
      geom_rug(alpha = .5) + geom_line(col = 'blue')
  )
}
