#include "hybrid_main.h"
#include "ui_hybrid_main.h"
#include <sstream>
#include <QMessageBox>
#include "cl_bridge_utils.hpp"
using ss=std::stringstream;
using std::cout;
using std::endl;
hybrid_main::hybrid_main(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::hybrid_main)
{
    ui->setupUi(this);
}


hybrid_main::~hybrid_main()
{
    delete ui;
}

/* int -> string */
auto itos=[](auto in){
    ss s;s<<in;string res;s>>res;
    return res;
};

/* when called, an alert dialog shows up */
auto jump_out_alert_window=[](std::string str){
    QMessageBox::critical(0 ,
                          "critical message" , QString::fromStdString(str),
                          QMessageBox::Ok | QMessageBox::Default ,
                          QMessageBox::Cancel | QMessageBox::Escape , 	0 );

};

/* concurrent fibonacci */
void hybrid_main::on_pushButton_clicked()
{
    auto str=ui->edit->text().toStdString();
    if(str==""){
        jump_out_alert_window("You haven't input anything!");
    } else {
        cl_obj rtv=cl_eval("pfib", str);
        string strt=itos(rtv.to_int());
        ui->ans->setText(QString::fromStdString(strt));
    }
}

/* quick sort. */
void hybrid_main::on_pushButton_2_clicked()
{
    auto str=ui->input->text().toStdString();
    if(str=="")
    {
        jump_out_alert_window("You haven't input anything!");
    } else {
        cout<<par_list(str)<<endl;
        cl_obj rtvl=cl_eval("qsort", par_list(str), "#'<");
        cout<<rtvl.car().to_int();
        string lab="";
        auto read_list_to_string=[&](auto elem){lab=lab+" "+itos(ecl_to_int(elem));};
        rtvl.list_traverse(read_list_to_string);
        ui->output->setText(QString::fromStdString(lab));
    }
}




/* hello lisp */
void hybrid_main::on_pushButton_3_clicked()
{
    string s=cl_obj(cl_eval("say-hello")).to_std_string();
    jump_out_alert_window(s);
}
