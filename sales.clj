(ns sales
  (:gen-class))
(require '[clojure.string :as str])

; Function to split data by |
(defn split_line [line]
  (str/split line #"\|")
)

;To parse data
(defn parseData[parsing]
  (if (re-find #"^-?\d+\.?\d*$" parsing)
   (read-string parsing)))

; Display Customer table------------------------------------------------------------------------
(defn displayCustomerTable[]
 (def readCustFile(slurp "cust.txt"))
 (def readLine (str/split readCustFile  #"\r\n"))
 (def splittedData (map(fn[y] (split_line y)) readLine))
 (def mergedData (into(sorted-map)(apply merge( map #(hash-map (Integer/parseInt (first %1)) (vec (rest %1))) splittedData))))
 (doseq [[k v] mergedData]
    (def getKey (str/replace k #":" ""))
    (print getKey" : [ ")(pr (get v 0))(print " ")(pr (get v 1))(print " ")(pr (get v 2)) (print " ]")
    (println)
)
)

; Display Product table-------------------------------------------------------------------------
(defn displayProductTable[]
 (def readProdFile(slurp "prod.txt"))
 (def readLine (str/split readProdFile  #"\r\n"))
 (def splittedData (map(fn[y] (split_line y)) readLine))
 (def mergedData (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt(first %1)) (vec (rest %1))) splittedData))))
 (doseq [[k v] mergedData]
    (def getKey (str/replace k #":" ""))
    (def getFirstVal(get v 0))
    (def getSecVal(get v 1))
    (def getThirdVal(get v 2))
    (print getKey": [ ")(pr getFirstVal)(print " ")(pr getSecVal)(print " ]")
    (println)
)
)

;Display Sales Table----------------------------------------------------------------------------
;Get Customer name for 3rd part
(defn GetCustName[ID]
     (def readCustFile(slurp "cust.txt"))
		(def readLine (str/split readCustFile  #"\r\n"))
		(def splittedData (map(fn[y] (split_line y)) readLine))
	  (def mergedData (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt(first %1)) (vec (rest %1))) splittedData))))
    (doseq [[k v] mergedData]
      (def ke (str k))
      (def idd (str ID))
      (if(= ke idd)
        (print "[" (get v 0) ",")        
        )
      ))

;Get Product name for 3rd part
(defn GetProduct[ID]
    (def readCustFile(slurp "prod.txt"))
		(def readLine (str/split readCustFile  #"\r\n"))
		(def splittedData (map(fn[y] (split_line y)) readLine))
	  (def mergedData (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt(first %1)) (vec (rest %1))) splittedData))))
    (doseq [[k v] mergedData]
      (def prodKey (str k))
      (def prodId (str ID))
      (if(= prodKey prodId)
        (print(get v 0) ","))
        
))

;Display data with Customer and Product name for 3rd part
(defn displaySalesTable[]      
	  (def readSalesFile(slurp "sales.txt"))
	  (def readLineSales (str/split readSalesFile  #"\r\n"))
	  (def splittedDataSales (map(fn[y] (split_line y)) readLineSales))
	  (def mergedDataSales (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt(first %1)) (vec (rest %1))) splittedDataSales))))
    (doseq [[k v] mergedDataSales]
      (def getKey (str/replace k #":" ""))
      (def  cn (get v 0))
      (def  pn (get v 1))
      (def  itemCount (get v 2))     
      (print getKey":")(def custName (GetCustName cn))
      (def prodName (GetProduct pn))
      (println itemCount "]" )             
)) 


;Display Total Sale according to customer name Table (4th part)--------------------------------------------
;Calculate total sale by multiplying no. of items
(defn calctotal[itemCount pIdinSales]
   (def readProdFile(slurp "prod.txt"))
		(def readLine (str/split readProdFile  #"\r\n"))
		(def splittedData (map(fn[y] (split_line y)) readLine))
		(def mergedData (into(sorted-map)(apply merge( map #(hash-map  (first %1) (vec (rest %1))) splittedData))))
		(doseq [[k v] mergedData]
		  (if(= pIdinSales k)
      (do
		   (def price (get v 1))
        (def multiplyPrice (* (parseData itemCount) (parseData price)))
)))multiplyPrice)

; Get product id to calculate total sale
(defn getProIdFromSales[custKey]
  (def finalValue 0.0)
  (def finalValue2 0.0)

(def readSalesFile(slurp "sales.txt"))
	(def readLineSales (str/split readSalesFile  #"\r\n"))
	(def splittedDataSales (map(fn[y] (split_line y)) readLineSales))
	(def mergedDataSales (into(sorted-map)(apply merge( map #(hash-map  (first %1) (vec (rest %1))) splittedDataSales))))
  (doseq [[k v] mergedDataSales]
    (def cIdinSales (get v 0))
    (def pIdinSales (get v 1))
    (def itemCount (get v 2))
    (if(= cIdinSales custKey)
      (do
      (def total (calctotal itemCount pIdinSales))
      (def finalValue (+ finalValue total))
      (def finaldata (format "%.2f" finalValue))
      (def finalValue2 finaldata))))
  finalValue2)

; Get Customer id to calculate total sale
(defn getCustId[enteredName]
  (def notValid 0.0)
  (def readCustFile(slurp "cust.txt"))
	(def readLine (str/split readCustFile  #"\r\n"))
	(def splittedData (map(fn[y] (split_line y)) readLine))
	(def mergedData (into(sorted-map)(apply merge( map #(hash-map  (first %1) (vec (rest %1))) splittedData))))
  (doseq [[k v] mergedData]
    (def cName (str(get v 0)))
    (if(= cName enteredName) 
      (do 
      (def cN (getProIdFromSales k))
      (def notValid cN))))
      (println enteredName ":$ " notValid)  
  )

; Enter Customer name for 4th part
(defn totalSaleOfCustomer []
(do (print "Enter Customer name:") (flush) (read-line)
  (let[enteredName (str(read-line))]
  (getCustId enteredName))))


;Get total Count of product(5th part)-------------------------------------------------------------
(defn getTotalCount[prodID]
  (def finalTotal_product 0)
  (def readSalesFile(slurp "sales.txt"))
	(def readLineSales (str/split readSalesFile  #"\r\n"))
	(def splittedDataSales (map(fn[y] (split_line y)) readLineSales))
	(def mergedDataSales (into(sorted-map)(apply merge( map #(hash-map  (first %1) (vec (rest %1))) splittedDataSales))))
  (doseq [[k v] mergedDataSales]
    (def pIdinSales (get v 1))
    (if(= prodID pIdinSales)
      (do
      (def totalCount (get v 2))
      (def finalTotal_product (+ finalTotal_product (parseData totalCount)))))
)finalTotal_product)

;Get Product id after entering the product name(5th part)
(defn getProdId[enterProdName]
  (def notValid 0)
  (def readCustFile(slurp "prod.txt"))
		(def readLine (str/split readCustFile  #"\r\n"))
		(def splittedData (map(fn[y] (split_line y)) readLine))
	  (def mergedData (into(sorted-map)(apply merge( map #(hash-map  (first %1) (vec (rest %1))) splittedData))))
    (doseq [[k v] mergedData]
     (def prodName (str(get v 0)))
    
     (if(= prodName enterProdName) 
      (do 
      (def cN (getTotalCount k))
      (def notValid cN))))
      (println enterProdName ":" notValid)
     )

;Enter product name(5th part)
(defn totalCountOfProd []
(do (print "Enter product name:") (flush) (read-line)
  (let[enterProdName (str(read-line))]
  (getProdId enterProdName))))


;Exit Function----------------------------------------------------------------------------
(defn exitApplication[]
    (println "Good Bye.")
    (. System exit 0))

;Options Menu----------------------------------------------------------------------------
(defn printMenuOptions[]
   (println "")
   (println "*** Sales Menu ***")
   (println "----------------------")
   (println "1.Display Customer Table")
   (println "2.Display Product Table")
   (println "3.Display Sales Table")
   (println "4.Total sales for Customer")
   (println "5.Total count for Sales")
   (println "6. Exit")
   (println "Enter an option?")
   (println "")
)

;Start Menu by default--------------------------------------------------------------------
(defn startmenu[]
    (printMenuOptions) ; call menu options from another function
    (def selectedMenuOption (read))
    (if (or (< selectedMenuOption 1) (> selectedMenuOption 6))
    (println "Invalid choice. Enter choice again"))
    (if (== selectedMenuOption 1)
      (displayCustomerTable))
    (if (== selectedMenuOption 2)
      (displayProductTable))
    (if (== selectedMenuOption 3)
      ;(println "Display sales table"))
        (displaySalesTable))
    (if (== selectedMenuOption 4)
        (totalSaleOfCustomer))
    (if (== selectedMenuOption 5)
        (totalCountOfProd))
    (if (== selectedMenuOption 6)
      (exitApplication))
      (recur)
    )
(startmenu)




