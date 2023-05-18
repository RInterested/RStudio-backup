require(ggplot2)

f = function(x) (1+1i)*log(sin((x^3-1)/x))
z = as.vector(outer(seq(-5, 5, by =.01), 1i*seq(-5, 5, by =.01), '+'))
z = z[is.finite(f(z))]

w=data.frame(x = Re(z),
             y = Im(z),
             h = ((Arg(f(z)) + (2*pi)) %% (2*pi))/(2*pi),
             s=(1+sin(2*pi*log(1+Mod(f(z)))))/2,
             v=(1+cos(2*pi*log(1+Mod(f(z)))))/2
              )

opt=theme(legend.position="none",
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text =element_blank())
ggplot(data=w, aes(x=x, y=y)) + geom_tile(fill=hsv(w$h,w$s,w$v))+ opt