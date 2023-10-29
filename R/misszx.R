#' Outpatient log statistics
#'
#' Statistics on the missing logs of outpatient clinics
#'
#' @param data Datasets that need to be analyzed
#'
#'
#' @return  Return the statistical data table
#' @examples
#' misszx(data)




misszx <- function(data) {
  quchongmzrz <- data[!duplicated(data$身份证号码),]  #根据身份证号去重
  quchongmzrz[is.na(quchongmzrz)] <- 0  #很奇怪，为什么联系方式导入后出现缺失值了呢
  mzks <- data[!duplicated(data$医生),] #去除重复的医生
  queshitj <- matrix(nrow=(nrow(mzks)),ncol=4) #统计行数（也就是门诊数），并生成“行”*4的矩阵
  queshitj <- data.frame(queshitj)  #将矩阵转换为数据框
  queshitj[is.na(queshitj)] <- 0 #将缺失值填充未0

  rownames <- (mzks[,"医生"])  #把每一行的医生名放入rownames中
  columnnames <- c("门诊量", "住址缺失数", "缺失职业统计", "联系方式缺失")
  names(queshitj) <- columnnames
  row.names(queshitj) <- rownames

  wxzz <- c("", "HD", "江苏省苏州市相城区", "江苏省苏州市相城区黄埭镇",  "苏州市相城区黄埭镇", "苏州市相城区",  "相城区黄埭镇",  "黄埭镇", "hd", c(0:10), "黄埭") #无效住址


  for ( n in 1:nrow(quchongmzrz))
    {
    for (i in 1:nrow(queshitj))
      {
      if(quchongmzrz[n,"医生"] == rownames(queshitj)[i])
        {
          queshitj[i,"门诊量"] <- queshitj[i,"门诊量"] + 1
        if (quchongmzrz[n,"现住址"] %in% wxzz)
          queshitj[i,"住址缺失数"] <- queshitj[i,"住址缺失数"] + 1
        if (quchongmzrz[n,"职业"] == "")
        queshitj[i,"缺失职业统计"] <- queshitj[i,"缺失职业统计"] + 1
        if (!(nchar(quchongmzrz[n,"联系电话"]) == 11 | nchar(quchongmzrz[n,"联系电话"]) == 8| nchar(quchongmzrz[n,"家庭电话"]) == 11 | nchar(quchongmzrz[n,"家庭电话"]) == 8 ))

      queshitj[i,"联系方式缺失"] <- queshitj[i,"联系方式缺失"] + 1
        }
      }
     }
  queshitj
}
