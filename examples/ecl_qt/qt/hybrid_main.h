#ifndef HYBRID_MAIN_H
#define HYBRID_MAIN_H

#include <QMainWindow>

namespace Ui {
class hybrid_main;
}

class hybrid_main : public QMainWindow
{
    Q_OBJECT

public:
    explicit hybrid_main(QWidget *parent = 0);
    ~hybrid_main();

private slots:
    void on_pushButton_clicked();

    void on_pushButton_2_clicked();

    void on_pushButton_3_clicked();

private:
    Ui::hybrid_main *ui;
};




#endif // HYBRID_MAIN_H
